///! Module with transpiler components. Allows transpiling Python source code to Rust source code.
pub(crate) mod identify_lines;
pub(crate) mod recontextualize;

use super::*;
use crate::error::Result;
use derive_deref::Deref;
use log::{debug, trace};
use num_bigint::BigInt;
use quote::ToTokens;
use recontextualize::recontextualize;
use rustpython_parser::ast::*;
use rustpython_parser::parser;
use syn;

/// Transpiles given Python source code to Rust
pub fn transpile_python(src: PySource) -> Result<String> {
    let generator = match src {
        PySource::Program(s, ProgramKind::Runnable) => {
            // Parse source into a program using RustPython
            let program = parser::parse_program(s)?;

            // Add back unsupported context like comments
            let py_lines = recontextualize(s, program)?;

            RsGenerator {
                py_program: py_lines,
            }
        }
        PySource::Program(_s, ProgramKind::NonRunnable) => {
            // TODO: interpret freestanding Python statements as const declarations
            unimplemented!()
        }
    };

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

/// Anything and everything required to create an expression in Rust. Has a close match to "a line"
/// in Python, but might span multiple lines, or contain other relevant contextual information.
pub(crate) enum PyNode {
    Statement(Located<StatementType>),
    Newline(Located<()>),
    Comment(Located<String>),
}

impl From<Located<StatementType>> for PyNode {
    fn from(stmt: Located<StatementType>) -> Self {
        PyNode::Statement(stmt)
    }
}

enum RsNode {
    Statement(syn::Stmt),
}

impl RsGenerator {
    fn generate(&self) -> Result<String> {
        let mut rs_nodes = vec![];

        for node in &self.py_program {
            let rs_node = match node {
                PyNode::Statement(stmt) => RsNode::Statement(visit_statement(&stmt)),
                _ => unimplemented!(),
            };
            rs_nodes.push(rs_node);
        }

        // Format Rust statements
        let formatted = rs_nodes
            .iter()
            .map(|node| match node {
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

                    relevant
                }
            })
            .flatten();

        // Catenate statements with newlines and return
        let out = formatted.fold(String::new(), |acc, next| acc + &next + "\n");

        Ok(out)
    }
}

#[derive(Clone, Deref)]
struct RsStmt(pub syn::Stmt);

impl From<Statement> for RsStmt {
    fn from(py_stmt: Statement) -> Self {
        let rs_stmt = visit_statement(&py_stmt);
        RsStmt(rs_stmt)
    }
}

pub fn visit_statement(stmt: &Statement) -> syn::Stmt {
    let _location = &stmt.location;
    let stmt = &stmt.node;
    trace!("visit: {:?}", stmt);

    use StatementType::*;
    match stmt {
        Assign { targets, value } => visit_assign(&targets, &value),
        Expression { expression } => syn::Stmt::Expr(visit_expression(&expression)),
        FunctionDef {
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
        Return { value } => syn::Stmt::Expr(visit_return(value.as_ref())),
        _ => {
            println!("unimplemented: {:?}", stmt);
            unimplemented!()
        }
    }
}

fn visit_return(value: Option<&Expression>) -> syn::Expr {
    let expr_return = syn::ExprReturn {
        attrs: vec![],
        return_token: <syn::Token![return]>::default(),
        expr: value.and_then(|e| Some(Box::new(visit_expression(&e)))),
    };
    syn::Expr::Return(expr_return)
}

fn visit_function_def(
    _is_async: bool,
    name: &str,
    args: &Box<Parameters>,
    body: &Suite,
    _decorator_list: &[Expression],
    _returns: Option<&Expression>,
) -> syn::Stmt {
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
        inputs: visit_params(args),
        variadic: None,
        output: syn::ReturnType::Default,
    };
    let block = visit_body(body); // {}
    let item_fn = syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Public(syn::VisPublic {
            pub_token: <syn::Token![pub]>::default(),
        }),
        sig: signature,
        block: Box::new(block),
    };
    let item = syn::Item::Fn(item_fn);
    syn::Stmt::Item(item)
}

fn visit_params(args: &Parameters) -> syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma> {
    // HACK: drop everything but the ordinary parameters for now
    let args = &args.args;

    let mut list = syn::punctuated::Punctuated::new();
    for ref arg in args {
        let rust_param = visit_param(arg);
        list.push(rust_param);
    }
    list
}

fn visit_param(arg: &Parameter) -> syn::FnArg {
    let _location = &arg.location;
    let id_pat = Pat::from(&arg.arg).0;
    let pat = syn::PatType {
        attrs: vec![],
        pat: Box::new(id_pat),
        colon_token: <syn::Token![:]>::default(),
        ty: Box::new(syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::new(),
            },
        })),
    };
    syn::FnArg::Typed(pat)
}

// Use newtype-pattern to add operations
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

fn visit_body(body: &Suite) -> syn::Block {
    let stmts = body
        .into_iter()
        .map(|py_stmt| visit_statement(&py_stmt))
        .collect::<Vec<syn::Stmt>>();
    syn::Block {
        brace_token: syn::token::Brace(proc_macro2::Span::call_site()),
        stmts,
    }
}

fn visit_assign(targets: &[Expression], value: &Expression) -> syn::Stmt {
    trace!("visit: Assign({:?}, {:?})", targets, value);
    let lhs_target = if targets.len() == 1 {
        let target = targets.first().unwrap();
        let _location = &target.location;
        let expr = &target.node;
        let ident = match expr {
            ExpressionType::Identifier { name } => {
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
            Box::new(visit_expression(value)),
        )),
        semi_token: <syn::Token![;]>::default(),
    };
    let local = syn::Stmt::Local(local);
    debug!("Assign({:?}, {:?}) -> {:?}", &targets, &value, &local);
    local
}

fn visit_expression(expr: &Expression) -> syn::Expr {
    trace!("visit: {:?}", expr);
    let _location = &expr.location;
    let expr = &expr.node;

    let rs_expr = match expr {
        ExpressionType::Number { value } => visit_number(&value),
        ExpressionType::Binop { a, op, b } => visit_binop(a, op, b),
        ExpressionType::Identifier { name } => syn::Expr::Path(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: {
                    let mut segs = syn::punctuated::Punctuated::new();
                    segs.push(syn::PathSegment {
                        ident: Ident::from(name).0,
                        arguments: syn::PathArguments::None,
                    });
                    segs
                },
            },
        }),
        ExpressionType::Call { function, args, .. } => syn::Expr::Call(syn::ExprCall {
            attrs: vec![],
            func: Box::new(visit_expression(&*function)),
            paren_token: syn::token::Paren(proc_macro2::Span::call_site()),
            args: visit_args(args),
        }),
        _ => {
            println!("unimplemented: {:?}", expr);
            unimplemented!()
        }
    };
    debug!("{:?} -> {:?}", &expr, &rs_expr);
    rs_expr
}

fn visit_args(args: &[Expression]) -> syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> {
    let mut list = syn::punctuated::Punctuated::new();
    for arg in args {
        let rust_arg = visit_expression(&arg);
        list.push(rust_arg);
    }
    list
}

fn visit_binop(a: &Box<Expression>, op: &Operator, b: &Box<Expression>) -> syn::Expr {
    let bin_expr = syn::ExprBinary {
        attrs: vec![],
        left: Box::new(visit_expression(&a)),
        op: visit_bin_op(op),
        right: Box::new(visit_expression(&b)),
    };
    syn::Expr::Binary(bin_expr)
}

fn visit_bin_op(op: &Operator) -> syn::BinOp {
    match op {
        Operator::Add => syn::BinOp::Add(<syn::Token![+]>::default()),
        _ => {
            println!("unimplemented: {:?}", op);
            unimplemented!()
        }
    }
}

fn visit_number(number: &Number) -> syn::Expr {
    trace!("visit: {:?}", number);
    use Number::*;
    let rs_number = match number {
        Integer { value } => visit_bigint(&value),
        _ => unimplemented!(),
    };
    debug!("{:?} -> {:?}", &number, rs_number);
    rs_number
}

fn visit_bigint(bigint: &BigInt) -> syn::Expr {
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
        _ => unimplemented!(),
    };
    debug!("{:?} -> {:?}", bigint, expr);
    expr
}
