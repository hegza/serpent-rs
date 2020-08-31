//! Defines conversions from Python AST nodes into Rust AST nodes.

use super::{dummy, util};
use crate::transpile::context::AstContext;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::{ast as rs, ptr::P};
use rustpython_parser::ast as py;

/// Like `std::convert::From` but for Python AST node conversion into Rust. Does
/// not have Into reciprocal.
pub(crate) trait FromPy<T>: Sized {
    fn from_py(_: &T, ctx: &mut AstContext) -> Self;
}

impl FromPy<py::Expression> for rs::PatKind {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        match &expr.node {
            py::ExpressionType::BoolOp { op, values } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Binop { a, op, b } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Subscript { a, b } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Unop { op, a } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Await { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Yield { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::YieldFrom { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Compare { vals, ops } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Attribute { value, name } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Number { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::List { elements } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Tuple { elements } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Dict { elements } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Set { elements } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Comprehension { kind, generators } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Starred { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Slice { elements } => ctx.unimplemented_pat(expr),
            py::ExpressionType::String { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Bytes { value } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Identifier { name } => util::str_to_pat_kind( name ),
            py::ExpressionType::Lambda { args, body } => ctx.unimplemented_pat(expr),
            py::ExpressionType::IfExpression { test, body, orelse } => ctx.unimplemented_pat(expr),
            py::ExpressionType::NamedExpression { left, right } => ctx.unimplemented_pat(expr),
            // The following Python expressions can not be Rust patterns
            py::ExpressionType::True | py::ExpressionType::False | py::ExpressionType::None | py::ExpressionType::Ellipsis => panic!(
                "{:?} cannot be made into a Rust pattern, it's also likely invalid Python code unless transpiler implementation has a mistake"
            ),
        }
    }
}

impl FromPy<py::Expression> for rs::Pat {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        rs::Pat {
            id: dummy::node_id(),
            kind: rs::PatKind::from_py(expr, ctx),
            span: dummy::span(),
        }
    }
}

impl FromPy<py::Expression> for rs::ExprKind {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        // TODO: expr.location ignored

        match &expr.node {
            py::ExpressionType::BoolOp { op, values } => ctx.unimplemented_item(expr),
            py::ExpressionType::Binop { a, op, b } => ctx.unimplemented_item(expr),
            py::ExpressionType::Subscript { a, b } => ctx.unimplemented_item(expr),
            py::ExpressionType::Unop { op, a } => ctx.unimplemented_item(expr),
            py::ExpressionType::Await { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Yield { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::YieldFrom { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Compare { vals, ops } => ctx.unimplemented_item(expr),
            py::ExpressionType::Attribute { value, name } => ctx.unimplemented_item(expr),
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => ctx.unimplemented_item(expr),
            py::ExpressionType::Number { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::List { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Tuple { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Dict { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Set { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Comprehension { kind, generators } => ctx.unimplemented_item(expr),
            py::ExpressionType::Starred { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Slice { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::String { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Bytes { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Identifier { name } => ctx.unimplemented_item(expr),
            py::ExpressionType::Lambda { args, body } => ctx.unimplemented_item(expr),
            py::ExpressionType::IfExpression { test, body, orelse } => ctx.unimplemented_item(expr),
            py::ExpressionType::NamedExpression { left, right } => ctx.unimplemented_item(expr),
            py::ExpressionType::True => ctx.unimplemented_item(expr),
            py::ExpressionType::False => ctx.unimplemented_item(expr),
            py::ExpressionType::None => ctx.unimplemented_item(expr),
            py::ExpressionType::Ellipsis => ctx.unimplemented_item(expr),
        };

        // HACK: Just return a placeholder expression for now
        let block = rs::Block {
            stmts: vec![],
            id: dummy::node_id(),
            rules: rs::BlockCheckMode::Default,
            span: dummy::span(),
        };
        rs::ExprKind::Block(P(block), None)
    }
}

impl FromPy<py::Expression> for rs::Expr {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        rs::Expr {
            id: dummy::node_id(),
            kind: rs::ExprKind::from_py(expr, ctx),
            span: dummy::span(),
            attrs: dummy::attr_vec(),
            // TODO: maybe store-transpiled tokens
            tokens: None,
        }
    }
}

/*
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
*/
