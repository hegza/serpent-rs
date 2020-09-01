//! TODO: Find all HACKs, FIXMEs
pub mod dummy;
mod from_py;
mod util;

use super::context::ImportKind;
use crate::error::TranspileNodeError;
use crate::transpile::{context::AstContext, python, rust};
use from_py::FromPy;
use log::warn;
use py::Located;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::{ast as rs, ptr::P};
use rustpython_parser::ast as py;
use std::slice::Iter;

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
        } => visit_import_from(level, module, names, ctx),
        py::StatementType::Pass => ctx.unimplemented_item(stmt),
        py::StatementType::Assert { test, msg } => ctx.unimplemented_item(stmt),
        py::StatementType::Delete { targets } => ctx.unimplemented_item(stmt),
        py::StatementType::Assign { targets, value } => visit_assign(targets, value, ctx),
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

/// Visits each of the import symbols in a Python import from statement.
///
/// Emits a `use crate::...` for identified, local imports and stores
/// unidentified, foreign imports for later processing
///
/// TODO: merge Python import trees into Rust use trees instead of emitting a
/// separate tree for each
fn visit_import_from(
    level: &usize,
    module: &Option<String>,
    names: &Vec<py::ImportSymbol>,
    ctx: &mut AstContext,
) {
    if *level != 0 {
        ctx.unimplemented_parameter("import_from", "level", level);
    }

    // Merge the module path into the symbol path by mapping the module Option
    let mut names_with_module = module.as_ref().map(|module| {
        names
            .iter()
            .map(|import_symbol| {
                let new_symbol = module.to_string() + "::" + &import_symbol.symbol;
                py::ImportSymbol {
                    symbol: new_symbol,
                    alias: import_symbol.alias.clone(),
                }
            })
            .collect::<Vec<py::ImportSymbol>>()
    });

    let names = match names_with_module {
        Some(ref names) => names,
        None => names,
    };

    for name in names {
        visit_import_symbol(name, ctx);
    }
}

/// Visits a Python import symbol with a symbol and an alias
///
/// Emits one of two things, either:
/// a) `use crate::{name.symbol} as {name.alias};` for identified local imports,
/// or b) `use {name.symbol} as {name.alias}` for unidentified foreign imports.
///
/// Foreign imports are also stored for later processing.
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

/// Visits a Python assign statement to emit a transpiled Rust assign statement.
/// TODO: figure out if the assign is
///     1. a re-assign,
///     2. a shadow,
///     3. or something else
/// TODO: Python assignment statement should often become a Rust assignment
/// expression wrapped in a statement (`rs::StmtKind::Expr()`)
///
/// For now, all assignments are assumed to be
/// new variables by default, becoming Locals in Rust.
fn visit_assign(targets: &[py::Expression], value: &py::Expression, ctx: &mut AstContext) {
    // Ignore multi-target assignments
    if targets.len() != 1 {
        ctx.unimplemented_parameter("assign", "targets", &targets);
    }

    let lhs = targets.first().unwrap();

    // Transform Python left-hand expression into a Rust pattern
    let pat = rs::Pat::from_py(lhs, ctx);

    // Transform the right-hand expression into the init value expression
    let init = rs::Expr::from_py(value, ctx);
    // An assign always has a rhs, thus the Rust Local always has an init
    let init = Some(P(init));

    let rust_stmt = rs::StmtKind::Local(P(create_local(pat, init)));
    let rust_node = rust::NodeKind::Stmt(rust_stmt);
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

fn create_local(pat: rs::Pat, init: Option<P<rs::Expr>>) -> rs::Local {
    rs::Local {
        id: dummy::node_id(),
        pat: P(pat),
        // Assume no type -> use type inference
        ty: None,
        init,
        span: dummy::span(),
        attrs: dummy::attr_vec(),
    }
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
    // TODO: can be used to store transpiled tokens for further processing
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
*/
