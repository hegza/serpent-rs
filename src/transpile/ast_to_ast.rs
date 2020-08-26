//! TODO: Find all HACKs
pub mod dummy;
mod util;

use super::context::ImportKind;
use crate::error::TranspileNodeError;
use crate::transpile::{context::AstContext, python, rust};
use log::warn;
use py::Located;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::{ast as rs, ptr::P};
use rustpython_parser::ast as py;

/// A type alias for `Result<T, serpent::error::TranspileNodeError>`.
pub type Result<T> = std::result::Result<T, TranspileNodeError>;

/// Represents something that can be fallibly transpiled into Rust. The
/// transpiled node will be added to the supplied transpilation context.
pub(crate) trait TranspileNode {
    fn transpile(&self, ctx: &mut AstContext) -> Result<()>;
}

/// Top-level implementation for transpiling Python nodes (statements, comments,
/// newlines) into Rust.
impl TranspileNode for python::NodeKind {
    fn transpile(&self, ctx: &mut AstContext) -> Result<()> {
        use python::NodeKind;

        match &self {
            // Recurse into subexpressions
            NodeKind::Statement(stmt) => {
                visit_statement(&stmt, ctx);
            }
            // A newline is a newline
            NodeKind::Newline(_loc) => {
                ctx.emit(rust::NodeKind::Newline);
            }
            // A comment is a comment
            NodeKind::Comment(Located { node, .. }) => {
                ctx.emit(rust::NodeKind::Comment(node.to_owned()));
            }
        }

        ctx.advance();
        Ok(())
    }
}

// <!-- AST transpilation implementations here          -->
// <!-- 1. Visit each different kind of Python AST node -->
// <!-- 2. Process py-AST nodes into into rs-AST nodes  -->
// <!-- 3. Construct a Rust item and emit               -->

/// Visits a Python statement to emit transpiled Rust.
fn visit_statement(stmt: &py::Statement, ctx: &mut AstContext) {
    match &stmt.node {
        py::StatementType::Break => ctx.unimplemented_item(stmt),
        py::StatementType::Continue => ctx.unimplemented_item(stmt),
        py::StatementType::Return { value } => visit_return(value, ctx),
        py::StatementType::Import { names } => visit_import(names, ctx),
        py::StatementType::ImportFrom {
            level,
            module,
            names,
        } => ctx.unimplemented_item(stmt),
        py::StatementType::Pass => ctx.unimplemented_item(stmt),
        py::StatementType::Assert { test, msg } => ctx.unimplemented_item(stmt),
        py::StatementType::Delete { targets } => ctx.unimplemented_item(stmt),
        // TODO: py::StatementType::Assign { targets, value } => visit_assign(targets, value, ctx),
        py::StatementType::Assign { targets, value } => ctx.unimplemented_item(stmt),
        py::StatementType::AugAssign { target, op, value } => ctx.unimplemented_item(stmt),
        py::StatementType::AnnAssign {
            target,
            annotation,
            value,
        } => ctx.unimplemented_item(stmt),
        // TODO: py::StatementType::Expression { expression } => visit_expression(expression, ctx),
        py::StatementType::Expression { expression } => ctx.unimplemented_item(stmt),
        py::StatementType::Global { names } => ctx.unimplemented_item(stmt),
        py::StatementType::Nonlocal { names } => ctx.unimplemented_item(stmt),
        py::StatementType::If { test, body, orelse } => ctx.unimplemented_item(stmt),
        py::StatementType::While { test, body, orelse } => ctx.unimplemented_item(stmt),
        py::StatementType::With {
            is_async,
            items,
            body,
        } => ctx.unimplemented_item(stmt),
        py::StatementType::For {
            is_async,
            target,
            iter,
            body,
            orelse,
        } => ctx.unimplemented_item(stmt),
        py::StatementType::Raise { exception, cause } => {}
        py::StatementType::Try {
            body,
            handlers,
            orelse,
            finalbody,
        } => ctx.unimplemented_item(stmt),
        py::StatementType::ClassDef {
            name,
            body,
            bases,
            keywords,
            decorator_list,
        } => ctx.unimplemented_item(stmt),
        py::StatementType::FunctionDef {
            is_async,
            name,
            args,
            body,
            decorator_list,
            returns,
        } => visit_function_def(is_async, name, args, body, decorator_list, returns, ctx),
    }
}

/// Visits a Python function definition to emit a transpiled Rust function
/// definition.
fn visit_function_def(
    is_async: &bool,
    name: &str,
    args: &Box<py::Parameters>,
    body: &py::Suite,
    decorator_list: &[py::Expression],
    returns: &Option<py::Expression>,
    ctx: &mut AstContext,
) {
    if !decorator_list.is_empty() {
        ctx.unimplemented_parameter("function_def", "decorator_list", &decorator_list);
    }

    let params = create_params_list(args, ctx);
    let ret = infer_return_type(args, body, returns, ctx);
    // HACK: assume no generics for now
    let generics = rs::Generics::default();

    // Generate statements in the body
    ctx.start_block();
    for stmt in body {
        visit_statement(stmt, ctx);
    }
    let block = ctx.finish_block();

    let fn_node = create_function_node(name, *is_async, params, ret, generics, block);

    ctx.emit(fn_node);
    /*
    // <--! OLD -->
    // signature, eg. `unsafe fn initialize(&self)`
    let signature = rs::Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: <rs::Token![fn]>::default(),
        ident: Ident::from(name).0,
        generics: rs::Generics {
            lt_token: None,
            params: rs::punctuated::Punctuated::new(),
            gt_token: None,
            where_clause: None,
        },
        paren_token: rs::token::Paren(proc_macro2::Span::call_site()),
        inputs: visit_params(args)?,
        variadic: None,
        output: rs::ReturnType::Default,
    };
    let block = visit_body(body, cursor)?; // { ... }
    let item_fn = rs::ItemFn {
        attrs: vec![],
        vis: rs::Visibility::Public(rs::VisPublic {
            pub_token: <rs::Token![pub]>::default(),
        }),
        sig: signature,
        block: Box::new(block),
    };
    let item = rs::Item::Fn(item_fn);
    Ok(rs::Stmt::Item(item))
    */
}

/// Visits a Python return statement to emit a transpiled Rust return statement.
///
/// TODO: merge Python import trees into Rust use trees instead of emitting a
/// separate tree for each
fn visit_return(value: &Option<py::Expression>, ctx: &mut AstContext) {
    ctx.unimplemented_parameter("return", "value", value);
}

/// Visits each of the import symbols in a Python import statement.
///
/// Emits a `use crate::...` for identified, local imports and stores
/// unidentified, foreign imports for later processing
///
/// TODO: merge Python import trees into Rust use trees instead of emitting a
/// separate tree for each
fn visit_import(names: &Vec<py::ImportSymbol>, ctx: &mut AstContext) {
    for name in names {
        visit_import_symbol(name, ctx);
    }
}

/// Visits a Python import symbol with a symbol and an alias
///
/// Emits a `use crate::...` for identified, local imports and stores
/// unidentified, foreign imports for later processing
fn visit_import_symbol(name: &py::ImportSymbol, ctx: &mut AstContext) {
    // De-structure import symbol
    let py::ImportSymbol { symbol, alias } = name;

    // Local or foreign import?
    let import_kind = ctx.identify_import(symbol);

    let rust_node = match import_kind {
        // Create a `use crate::...` for identified local import
        ImportKind::Local => create_use_node(true, symbol, alias.as_ref()),
        ImportKind::Foreign => create_use_node(false, symbol, alias.as_ref()),
    };
    ctx.emit(rust_node);
}

/// Reads a function definition and its block to determine a return type
fn infer_return_type(
    args: &Box<py::Parameters>,
    body: &py::Suite,
    returns: &Option<py::Expression>,
    ctx: &mut AstContext,
) -> rs::FnRetTy {
    // `returns` contains the possible explicit return type of the Python function
    // definition
    match returns {
        // None == implicit return type
        None => {
            // TODO: it may or may not be possible to infer the omitted return
            // type
        }
        // Some(expr) == explicit return type
        Some(expr) => ctx.unimplemented_parameter("return_type", "returns", returns),
    }

    // HACK: set all return types as inferrable, this is probably illegal in Rust
    // AST
    let ty = rs::TyKind::Infer;
    let ty = P(rs::Ty {
        id: dummy::node_id(),
        kind: ty,
        span: dummy::span(),
    });

    rs::FnRetTy::Ty(ty)
}

fn create_params_list(args: &Box<py::Parameters>, ctx: &mut AstContext) -> Vec<rs::Param> {
    let py::Parameters {
        posonlyargs_count,
        args,
        kwonlyargs,
        vararg,
        kwarg,
        defaults,
        kw_defaults,
    } = &**args;

    let mut list = Vec::with_capacity(args.len());
    for arg in args {
        let rs_param = create_param(arg, ctx);
        list.push(rs_param);
    }
    list
}

fn create_param(param: &py::Parameter, ctx: &mut AstContext) -> rs::Param {
    // Report unimplemented AST information
    if param.annotation.is_some() {
        ctx.unimplemented_parameter("param", "param.annotation", &param.annotation);
    }

    let pat = P(util::str_to_pat(&param.arg));

    // HACK: set all parameter types as inferrable, this is probably illegal in Rust
    // AST
    let ty = rs::TyKind::Infer;
    let ty = P(rs::Ty {
        id: dummy::node_id(),
        kind: ty,
        span: dummy::span(),
    });

    rs::Param {
        // Assume no attributes
        attrs: dummy::attr_vec(),
        ty,
        pat,
        id: dummy::node_id(),
        span: dummy::span(),
        // HACK: set all parameters as placeholder
        is_placeholder: true,
    }
}

fn create_use_node(is_local: bool, symbol: &str, alias: Option<&String>) -> rust::NodeKind {
    // Map Python alias into Rust Ident
    let alias = alias.map(|s| util::ident(s));

    // Construct Rust use tree

    let mut segments = vec![];
    if is_local {
        segments.push(rs::PathSegment::from_ident(util::ident("crate")));
    }
    segments.push(rs::PathSegment::from_ident(util::ident(&symbol)));

    let prefix = rs::Path {
        span: dummy::span(),
        segments,
    };
    let kind = rs::UseTreeKind::Simple(alias, dummy::node_id(), dummy::node_id());
    let span = dummy::span();

    let use_tree = rs::UseTree { prefix, kind, span };

    let rust_item = rs::ItemKind::Use(P(use_tree));
    let rust_node = rust::NodeKind::Item(rust_item);

    rust_node
}

fn create_function_node(
    ident: &str,
    is_async: bool,
    params: Vec<rs::Param>,
    ret: rs::FnRetTy,
    generics: rs::Generics,
    block: Option<P<rs::Block>>,
) -> rust::NodeKind {
    // Function signature: comprises metadata (header) and decl
    let header = rs::FnHeader {
        // NOTE: Transpiled functions are safe by default
        unsafety: rs::Unsafe::No,
        asyncness: if is_async {
            rs::Async::Yes {
                span: dummy::span(),
                closure_id: dummy::node_id(),
                return_impl_trait_id: dummy::node_id(),
            }
        } else {
            rs::Async::No
        },
        // NOTE: Transpiled functions are NonConst by default
        constness: rs::Const::No,
        // NOTE: Transpiled functions are not extern by default
        ext: rs::Extern::None,
    };
    let decl = P(rs::FnDecl {
        inputs: params,
        output: ret,
    });

    let defaultness = rs::Defaultness::Final;
    let fn_sig = rs::FnSig { header, decl };

    // NOTE: Assume no attributes
    let attrs = vec![];

    // HACK: Make all items public by default
    let vis = rs::VisibilityKind::Public;
    let vis = rustc_ap_rustc_span::source_map::Spanned {
        node: vis,
        span: dummy::span(),
    };
    // TODO: here is an opportunity to take a token stream as a parameter for more
    // effective fidelity printing
    let tokens = None;

    let rust_item = rs::ItemKind::Fn(defaultness, fn_sig, generics, block);
    let rust_named_item = rs::Item {
        attrs,
        id: dummy::node_id(),
        span: dummy::span(),
        vis,
        ident: util::ident(ident),
        kind: rust_item,
        tokens,
    };
    let rust_node = rust::NodeKind::ExtendedItem(rust_named_item);

    rust_node
}

/*
/// Visits a Python assign statement to emit a transpiled Rust assign statement.
fn visit_assign(targets: &[py::Expression], value: &py::Expression, ctx: &mut AstContext) -> Result<rs::Stmt> {
    let lhs_target = if targets.len() == 1 {
        let target = targets.first().unwrap();
        let _location = &target.location;
        let expr = &target.node;
        let ident = match expr {
            py::ExpressionType::Identifier { name } => {
                proc_macro2::Ident::new(name, proc_macro2::Span::call_site())
            }
            _ => unimplemented!(),
        };

        // Binding identifier pattern
        rs::Pat::Ident(rs::PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident,
            subpat: None,
        })
    } else {
        unimplemented!()
    };
    rs::Pat::Wild(rs::PatWild {
        attrs: Vec::new(),
        underscore_token: <rs::Token![_]>::default(),
    });

    // TODO: Python 'assign' could be a Local or an assignment expression in Rust
    // HACK: Used Local for now
    let local = rs::Local {
        attrs: Vec::new(),
        let_token: <rs::Token![let]>::default(),
        pat: lhs_target,
        init: Some((
            <rs::Token![=]>::default(),
            Box::new(Expression(value).visit()?),
        )),
        semi_token: <rs::Token![;]>::default(),
    };
    let local = rs::Stmt::Local(local);
    debug!("Assign({:?}, {:?}) -> {:?}", &targets, &value, &local);
    Ok(local)
}

fn visit_expression(expr: &py::ExpressionType) -> Result<rs::Expr> {
    let location = &expr.location;
    let expr = &expr.node;

    let rs_expr = match expr {
        py::ExpressionType::Number { value } => visit_number(&value)?,
        py::ExpressionType::Binop { a, op, b } => BinOp {
            left: a,
            op,
            right: b,
        }
        .visit()?,
        py::ExpressionType::Identifier { name } => syn::Expr::Path(Path(name).visit()?),
        py::ExpressionType::Call { function, args, .. } => syn::Expr::Call(syn::ExprCall {
            attrs: vec![],
            func: Box::new(visit_expression(&*function)),
            paren_token: syn::token::Paren(proc_macro2::Span::call_site()),
            args: visit_args(args)?,
        }),
        py::ExpressionType::String { value } => StringGroup(value).visit()?,
        _ => {
            println!("unimplemented: {:?}\nlocation: {:?}", expr, location);
            unimplemented!()
        }
    };
    Ok(rs_expr)
}

// TODO: add indentation information somehow
fn visit_body(body: &py::Suite, cursor: Option<&Cursor<PyNode>>) -> Result<rs::Block> {
    let stmts = body
        .into_iter()
        .map(|py_stmt| visit_statement(&py_stmt.node, Some(&py_stmt.location), cursor))
        .collect::<Result<Vec<rs::Stmt>>>()?;
    Ok(rs::Block {
        brace_token: rs::token::Brace(proc_macro2::Span::call_site()),
        stmts,
    })
}


fn visit_number(number: &py::Number) -> Result<rs::Expr> {
    trace!("visit: {:?}", number);
    use py::Number::*;
    let rs_number = match number {
        Integer { value } => visit_bigint(value)?,
        _ => unimplemented!(),
    };
    debug!("{:?} -> {:?}", &number, rs_number);
    Ok(rs_number)
}

fn visit_bigint(bigint: &BigInt) -> Result<rs::Expr> {
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
            rs::Expr::Lit(rs::ExprLit {
                attrs: Vec::new(),
                lit: rs::Lit::new(literal),
            })
        }
        _ => return Err(TranspileNodeError::unimplemented(bigint, None)),
    };
    debug!("{:?} -> {:?}", bigint, expr);
    Ok(expr)
}

impl<S> From<S> for Pat
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        Pat(rs::Pat::Ident(rs::PatIdent {
            attrs: vec![],
            by_ref: None,
            mutability: None,
            ident: Ident::from(s).0,
            subpat: None,
        }))
    }
}
*/
