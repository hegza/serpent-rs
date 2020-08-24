use crate::error::TranspileNodeError;
use crate::transpile::{context::AstContext, python, rust};
use py::Located;
use rustpython_parser::ast as py;

/// A type alias for `Result<T, serpent::error::TranspileNodeError>`.
pub type Result<T> = std::result::Result<T, TranspileNodeError>;

/// Represents something that can be fallibly transpiled into Rust. The
/// transpiled node will be added to the supplied transpilation AstContext.
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

// <!-- AST transpilation implementations here -->

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
        } =>
        // TODO: visit_function_def(is_async, name, args, body, decorator_list, returns),
        {
            ctx.unimplemented_item(stmt)
        }
    }
}

/// Visits a Python return statement to emit a transpiled Rust return statement.
fn visit_return(value: &Option<py::Expression>, ctx: &mut AstContext) {
    match value {
        Some(expression) => ctx.unimplemented_parameter("return", value),
        None => ctx.unimplemented_parameter("return", value),
    }
}

/// Visits a Python import statement.
///
/// Emits a `use crate::...` for identified, local imports and stores
/// unidentified, foreign imports for later processing
fn visit_import(names: &Vec<py::ImportSymbol>, ctx: &mut AstContext) {
    // TODO: figure out if the import symbol is A) a known local import, or B) a
    // foreign import, then either A) create Rust `use` to match, B)
    // `register_foreign_import() for later processing`
    ctx.unimplemented_parameter("import", names);
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


fn visit_function_def(
    _is_async: bool,
    name: &str,
    args: &Box<py::Parameters>,
    body: &py::Suite,
    _decorator_list: &[py::Expression],
    _returns: Option<&py::Expression>,
    cursor: Option<&Cursor<PyNode>>,
) -> Result<rs::Stmt> {
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
}

fn visit_params(
    args: &py::Parameters,
) -> Result<rs::punctuated::Punctuated<rs::FnArg, rs::token::Comma>> {
    // HACK: drop everything but the ordinary parameters for now
    let args = &args.args;

    let mut list = rs::punctuated::Punctuated::new();
    for ref arg in args {
        let rust_param = Parameter(&arg).visit()?;
        list.push(rust_param);
    }
    Ok(list)
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



fn visit_args(
    args: &[py::Expression],
) -> Result<rs::punctuated::Punctuated<rs::Expr, rs::token::Comma>> {
    let mut list = rs::punctuated::Punctuated::new();
    for arg in args {
        let rust_arg = Expression(&arg).visit()?;
        list.push(rust_arg);
    }
    Ok(list)
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

// Use newtype-pattern to add From<str>
#[derive(Deref)]
struct Ident(rs::Ident);

impl<S> From<S> for Ident
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        let s = s.as_ref();
        Ident(rs::Ident::new(s, proc_macro2::Span::call_site()))
    }
}

// Use newtype-pattern to add From<str>
#[derive(Deref)]
struct Pat(rs::Pat);

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
