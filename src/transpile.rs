//! Module with transpiler components. Allows transpiling Python source code to Rust source code.
pub(crate) mod identify_lines;
pub(crate) mod recontextualize;
pub(crate) mod visit;

use super::*;
use crate::error::TranspileError;
use recontextualize::recontextualize;
use visit::*;

use derive_deref::Deref;
use log::{debug, info, trace};
use num_bigint::BigInt;
use quote::ToTokens;
use rustpython_parser::ast;
use rustpython_parser::parser;
use std::convert::TryFrom;
use std::result;
use syn;

/// A type alias for `Result<T, serpent::TranspileError>`.
pub type Result<T> = result::Result<T, TranspileError>;

/// Transpiles given Python source code to Rust source code.
pub fn transpile_python(src: PySource) -> crate::error::Result<String> {
    // Create a Rust source-code generator by collecting information from the Python source code
    let generator = match src {
        PySource::Program(s, ProgramKind::Runnable) => {
            // Parse source into a program using RustPython
            info!("Parsing Python program...");
            let program = parser::parse_program(s)?;

            // Add back unsupported context like comments
            info!("Recontextualizing Python program...");
            let py_nodes = recontextualize(s, program)?;
            RsGenerator {
                py_program: py_nodes,
            }
        }
        PySource::Program(_s, ProgramKind::NonRunnable) => {
            // TODO: interpret freestanding Python statements as const declarations
            unimplemented!()
        }
    };

    // Generate the Rust source code with the information collected from the Python source files
    info!("Generating Rust source code...");
    generator.generate()
}

/// Represents what's required to generate a Rust program from Python source.
///
/// Call .generate() to produce a program.
struct RsGenerator {
    /// The original Python program parsed as a vector of nodes, containing single or multiline
    /// statements and unknown entries (whitespace, comments, other). Contains location as context
    /// for each entry.
    py_program: Vec<PyNode>,
}

impl RsGenerator {
    /// Generates the Rust source code
    pub fn generate(&self) -> crate::error::Result<String> {
        // Create a Rust node for each Python node
        let rs_nodes = self.translate_ast()?;

        // Generate the Rust source code from the Rust AST nodes
        let rs_src = RsGenerator::codegen(&rs_nodes);

        Ok(rs_src)
    }

    /// Converts the Python AST nodes into Rust AST nodes.
    fn translate_ast(&self) -> crate::error::Result<Vec<RsNode>> {
        let mut rs_nodes = vec![];
        let py_nodes = self.py_program.iter();
        for (node_no, &PyNode { ref src, ref node }) in py_nodes.enumerate() {
            info!("Node {}: `{}`", node_no, src);
            let rs_node = match node {
                PyNodeKind::Statement(stmt) => match visit_statement(&stmt) {
                    Ok(rs_stmt) => RsNode::Statement(rs_stmt),
                    Err(err) => {
                        return Err(crate::Error::new(ErrorKind::Transpile {
                            line: src.to_string(),
                            line_no: node_no,
                            reason: err,
                        }))
                    }
                },
                PyNodeKind::Newline(_located) => RsNode::Newline,
                PyNodeKind::Comment(ast::Located { node, .. }) => RsNode::Comment(node.clone()),
            };
            debug!("Node {} -> {:?}", node_no, &rs_node);
            rs_nodes.push(rs_node);
        }
        Ok(rs_nodes)
    }

    /// Generates Rust source code from Rust AST nodes.
    fn codegen(rs_nodes: &[RsNode]) -> String {
        // Format/print Rust statements
        let formatted = rs_nodes
            .iter()
            .enumerate()
            .map(|(node_no, node)| match node {
                RsNode::Statement(stmt) => {
                    let tokens = stmt.to_token_stream();

                    // HACK: Create a mock 'main' item to get rustfmt to accept it
                    let mock_source = "fn mock() {".to_owned() + &tokens.to_string() + "}";

                    // Format using rustfmt
                    let (_, file_map, _) = rustfmt::format_input::<Vec<u8>>(
                        rustfmt::Input::Text(mock_source),
                        &rustfmt::config::Config::default(),
                        None,
                    )
                    .unwrap();

                    let output = &file_map.first().unwrap().1;
                    let output_str = output.chars().map(|(c, _)| c).collect::<String>();

                    // Take all but the first and last line from the rustfmt output
                    let mut relevant = output_str
                        .lines()
                        .skip(1)
                        .map(|x| x.trim().to_owned())
                        .collect::<Vec<String>>();
                    relevant.pop();
                    info!("Node {} = Stmt::{:?} -> {:?}", node_no, stmt, &relevant);

                    relevant
                }
                RsNode::Newline => vec!["".to_owned()],
                RsNode::Comment(s) => vec![format!("// {}", s)],
            })
            .flatten();

        // Insert main
        let with_main = std::iter::once("fn main() {".to_owned())
            .chain(formatted)
            .chain(std::iter::once("}".to_owned()));

        // Catenate statements with newlines and return
        with_main.fold(String::new(), |acc, next| acc + &next + "\n")
    }
}

/// A statement, newline or comment of Python with associated context. Everything that's required
/// to create an expression in Rust.
#[derive(Debug)]
pub(crate) struct PyNode {
    src: String,
    node: PyNodeKind,
}

#[derive(Debug)]
pub(crate) enum PyNodeKind {
    Statement(ast::Located<ast::StatementType>),
    Newline(ast::Located<()>),
    Comment(ast::Located<String>),
}

impl PyNode {
    pub(crate) fn new(src: String, node: PyNodeKind) -> PyNode {
        PyNode { src, node }
    }
}

/// Convert a Python AST statement into a `PyNodeKind::Statement`.
impl From<ast::Located<ast::StatementType>> for PyNodeKind {
    fn from(stmt: ast::Located<ast::StatementType>) -> Self {
        PyNodeKind::Statement(stmt)
    }
}

#[derive(Debug)]
enum RsNode {
    Statement(syn::Stmt),
    Newline,
    Comment(String),
}

#[derive(Clone, Deref)]
struct RsStmt(pub syn::Stmt);

impl TryFrom<&ast::Statement> for RsStmt {
    type Error = TranspileError;

    fn try_from(py_stmt: &ast::Statement) -> Result<Self> {
        let rs_stmt = visit_statement(py_stmt)?;
        Ok(RsStmt(rs_stmt))
    }
}

/// Returns the Rust AST node.
///
/// Note that if the parameter is an expression statement, a `Stmt::Semi` is returned instead of an
/// `Stmt::Expr`. This is the correct functionality.
pub fn visit_statement(stmt: &ast::Statement) -> Result<syn::Stmt> {
    let _location = &stmt.location;
    let stmt = &stmt.node;
    trace!("visit: {:?}", stmt);

    match stmt {
        ast::StatementType::Assign { targets, value } => visit_assign(&targets, &value),
        ast::StatementType::Expression { expression } => Ok(syn::Stmt::Semi(
            Expression(&expression).visit()?,
            <syn::Token![;]>::default(),
        )),
        ast::StatementType::FunctionDef {
            is_async,
            name,
            args,
            body,
            decorator_list,
            returns,
        } => visit_function_def(
            *is_async,
            name,
            args,
            body,
            decorator_list,
            returns.as_ref(),
        ),
        ast::StatementType::Return { value } => Ok(syn::Stmt::Semi(
            visit_return(value.as_ref())?,
            <syn::Token![;]>::default(),
        )),
        _stmt => unimplemented!(),
    }
}

fn visit_return(value: Option<&ast::Expression>) -> Result<syn::Expr> {
    let expr = value.and(Some(Box::new(Expression(&value.unwrap()).visit()?)));
    let expr_return = syn::ExprReturn {
        attrs: vec![],
        return_token: <syn::Token![return]>::default(),
        expr,
    };
    Ok(syn::Expr::Return(expr_return))
}

fn visit_function_def(
    _is_async: bool,
    name: &str,
    args: &Box<ast::Parameters>,
    body: &ast::Suite,
    _decorator_list: &[ast::Expression],
    _returns: Option<&ast::Expression>,
) -> Result<syn::Stmt> {
    // signature, eg. `unsafe fn initialize(&self)`
    let signature = syn::Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: <syn::Token![fn]>::default(),
        ident: Ident::from(name).0,
        generics: syn::Generics {
            lt_token: None,
            params: syn::punctuated::Punctuated::new(),
            gt_token: None,
            where_clause: None,
        },
        paren_token: syn::token::Paren(proc_macro2::Span::call_site()),
        inputs: visit_params(args)?,
        variadic: None,
        output: syn::ReturnType::Default,
    };
    let block = visit_body(body)?; // { ... }
    let item_fn = syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Public(syn::VisPublic {
            pub_token: <syn::Token![pub]>::default(),
        }),
        sig: signature,
        block: Box::new(block),
    };
    let item = syn::Item::Fn(item_fn);
    Ok(syn::Stmt::Item(item))
}

fn visit_params(
    args: &ast::Parameters,
) -> Result<syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>> {
    // HACK: drop everything but the ordinary parameters for now
    let args = &args.args;

    let mut list = syn::punctuated::Punctuated::new();
    for ref arg in args {
        let rust_param = Parameter(&arg).visit()?;
        list.push(rust_param);
    }
    Ok(list)
}

// TODO: add indentation information somehow
fn visit_body(body: &ast::Suite) -> Result<syn::Block> {
    let stmts = body
        .into_iter()
        .map(|py_stmt| visit_statement(&py_stmt))
        .collect::<Result<Vec<syn::Stmt>>>()?;
    Ok(syn::Block {
        brace_token: syn::token::Brace(proc_macro2::Span::call_site()),
        stmts,
    })
}

fn visit_assign(targets: &[ast::Expression], value: &ast::Expression) -> Result<syn::Stmt> {
    trace!("visit: Assign({:?}, {:?})", targets, value);
    let lhs_target = if targets.len() == 1 {
        let target = targets.first().unwrap();
        let _location = &target.location;
        let expr = &target.node;
        let ident = match expr {
            ast::ExpressionType::Identifier { name } => {
                proc_macro2::Ident::new(name, proc_macro2::Span::call_site())
            }
            _ => unimplemented!(),
        };

        // Binding identifier pattern
        syn::Pat::Ident(syn::PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident,
            subpat: None,
        })
    } else {
        unimplemented!()
    };
    syn::Pat::Wild(syn::PatWild {
        attrs: Vec::new(),
        underscore_token: <syn::Token![_]>::default(),
    });

    // TODO: Python 'assign' could be a Local or an assignment expression in Rust
    // HACK: Used Local for now
    let local = syn::Local {
        attrs: Vec::new(),
        let_token: <syn::Token![let]>::default(),
        pat: lhs_target,
        init: Some((
            <syn::Token![=]>::default(),
            Box::new(Expression(value).visit()?),
        )),
        semi_token: <syn::Token![;]>::default(),
    };
    let local = syn::Stmt::Local(local);
    debug!("Assign({:?}, {:?}) -> {:?}", &targets, &value, &local);
    Ok(local)
}

fn visit_args(
    args: &[ast::Expression],
) -> Result<syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>> {
    let mut list = syn::punctuated::Punctuated::new();
    for arg in args {
        let rust_arg = Expression(&arg).visit()?;
        list.push(rust_arg);
    }
    Ok(list)
}

fn visit_number(number: &ast::Number) -> Result<syn::Expr> {
    trace!("visit: {:?}", number);
    use ast::Number::*;
    let rs_number = match number {
        Integer { value } => visit_bigint(&value)?,
        _ => unimplemented!(),
    };
    debug!("{:?} -> {:?}", &number, rs_number);
    Ok(rs_number)
}

fn visit_bigint(bigint: &BigInt) -> Result<syn::Expr> {
    trace!("visit: {:?}", bigint);
    let (sign, data) = bigint.to_u32_digits();
    let value: u32 = {
        if data.len() == 1 {
            *data.first().unwrap()
        } else {
            unimplemented!()
        }
    };
    let expr = match sign {
        num_bigint::Sign::Plus => {
            let literal = proc_macro2::Literal::u32_unsuffixed(value);
            syn::Expr::Lit(syn::ExprLit {
                attrs: Vec::new(),
                lit: syn::Lit::new(literal),
            })
        }
        _ => return Err(TranspileError::unimplemented(bigint, None)),
    };
    debug!("{:?} -> {:?}", bigint, expr);
    Ok(expr)
}

// Use newtype-pattern to add From<str>
#[derive(Deref)]
struct Ident(syn::Ident);

impl<S> From<S> for Ident
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        let s = s.as_ref();
        Ident(syn::Ident::new(s, proc_macro2::Span::call_site()))
    }
}

// Use newtype-pattern to add From<str>
#[derive(Deref)]
struct Pat(syn::Pat);

impl<S> From<S> for Pat
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        Pat(syn::Pat::Ident(syn::PatIdent {
            attrs: vec![],
            by_ref: None,
            mutability: None,
            ident: Ident::from(s).0,
            subpat: None,
        }))
    }
}
