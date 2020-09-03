//! Defines conversions from Python AST nodes into Rust AST nodes.
//!
//! TODO: Review all HACKs, FIXMEs

use super::{dummy, util};
use crate::transpile::context::AstContext;
use log::warn;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ap_rustc_span::source_map::Spanned;
use rustc_ast::{ast as rs, ptr::P, token};
use rustpython_parser::ast as py;

/// Like `std::convert::From` but for Python AST node conversion into Rust. Does
/// not have Into reciprocal. Should be implemented for relatively easy
/// conversions, more complex transformations might require more context.
pub(crate) trait FromPy<T>: Sized {
    fn from_py(_: &T, ctx: &mut AstContext) -> Self;
}

impl FromPy<py::Expression> for rs::ExprKind {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        match &expr.node {
            py::ExpressionType::BoolOp { op, values } => ctx.unimplemented_item(expr),
            py::ExpressionType::Binop { a, op, b } => {
                return into_rs_bin_op(BinOp::from_py(op, ctx), a, b, ctx)
            }
            py::ExpressionType::Subscript { a, b } => ctx.unimplemented_item(expr),
            py::ExpressionType::Unop { op, a } => {
                return into_rs_un_op(UnOp::from_py(op, ctx), a, ctx)
            }
            py::ExpressionType::Await { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Yield { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::YieldFrom { value } => ctx.unimplemented_item(expr),
            // A Python chained comparison, eg. `a < b` or `a < b < c`
            py::ExpressionType::Compare { vals, ops } => {
                if vals.len() != 2 || ops.len() != 1 {
                    ctx.unimplemented_parameter("chained_comparison", "(vals, ops)", &(vals, ops));
                }
                let op = ops.first().unwrap();
                let (a, b) = (&vals[0], &vals[1]);
                return into_rs_bin_op(BinOp::from_py(op, ctx), a, b, ctx);
            }
            // An attribute maps to either a rs::ExprKind::MethodCall or a rs::ExprKind::Field, this
            // can be determined by walking the AST in a late pass
            py::ExpressionType::Attribute { value, name } => {
                return attribute_into_rs(value, name, ctx)
            }
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => return into_rs_call(function, args, keywords, ctx),
            py::ExpressionType::Number { value } => {
                return rs::ExprKind::Lit(rs::Lit::from_py(value, ctx))
            }
            py::ExpressionType::List { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Tuple { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Dict { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Set { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::Comprehension { kind, generators } => ctx.unimplemented_item(expr),
            py::ExpressionType::Starred { value } => ctx.unimplemented_item(expr),
            py::ExpressionType::Slice { elements } => ctx.unimplemented_item(expr),
            py::ExpressionType::String { value } => {
                return rs::ExprKind::Lit(rs::Lit::from_py(value, ctx))
            }
            py::ExpressionType::Bytes { value } => ctx.unimplemented_item(expr),
            // An identifier in an expression is probably a rs::Path
            py::ExpressionType::Identifier { name } => return id_to_path(name, ctx),
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

// An attribute maps to either a rs::ExprKind::MethodCall or a
// rs::ExprKind::Field, this can be determined by walking the AST in a late pass
fn attribute_into_rs(
    value: &Box<py::Expression>,
    name: &str,
    ctx: &mut AstContext,
) -> rs::ExprKind {
    // HACK: assume all attributes are foreign method accesses
    //ctx.unimplemented_parameter("attribute", "(value, name)", &(value, name));

    // rs::ExprKind::MethodCall:
    //
    // The first element of the vector of an Expr is the expression that evaluates
    // to the object on which the method is being called on (the receiver), and the
    // remaining elements are the rest of the arguments.
    let receiver = rs::Expr::from_py(value, ctx);

    // The PathSegment represents the method name and its generic arguments (within
    // the angle brackets).
    let method_name = name;
    let seg = rs::PathSegment::from_ident(util::ident(method_name));

    rs::ExprKind::MethodCall(seg, vec![P(receiver)], dummy::span())
}

fn id_to_path(id: &str, ctx: &mut AstContext) -> rs::ExprKind {
    let path = util::str_to_path(id);

    // FIXME: Paths are never qualified (first arg = None)
    rs::ExprKind::Path(None, path)
}

fn into_rs_un_op(kind: UnOp, a: &Box<py::Expression>, ctx: &mut AstContext) -> rs::ExprKind {
    let a = rs::Expr::from_py(a, ctx);

    match kind {
        UnOp::Ast(op) => rs::ExprKind::Unary(op, P(a)),
        // Just return the operand itself on identity operation
        UnOp::Identity => a.kind,
        UnOp::Inversion => {
            // TODO: Inversion is only defined for integers, so we could check
            // for that in here

            //  The unary ~ (invert) operator yields the bit-wise inversion of
            // its plain or long integer argument. The bit-wise inversion of x
            // is defined as -(x+1). It only applies to integral numbers.
            let one_kind = rs::LitKind::Int(1, rs::LitIntType::Unsuffixed);
            let one = dummy::expr(rs::ExprKind::Lit(rs::Lit {
                token: dummy::token(one_kind.clone()),
                kind: one_kind,
                span: dummy::span(),
            }));
            let x_plus_one = dummy::expr(rs::ExprKind::Binary(
                Spanned {
                    node: rs::BinOpKind::Add,
                    span: dummy::span(),
                },
                P(a),
                P(one),
            ));

            rs::ExprKind::Unary(rs::UnOp::Not, P(x_plus_one))
        }
    }
}

fn into_rs_bin_op(
    kind: BinOp,
    a: &py::Expression,
    b: &py::Expression,
    ctx: &mut AstContext,
) -> rs::ExprKind {
    let a = P(rs::Expr::from_py(a, ctx));
    let b = P(rs::Expr::from_py(b, ctx));

    let op = match kind {
        BinOp::Ast(op) => op,
        BinOp::Method(method) => match method {
            BinMethod::Powi => {
                // HACK: just throwing in a `std::f64::powi` here without thinking about it too
                // much
                let kind = id_to_path("std::f64::powi", ctx);
                let func = rs::Expr {
                    id: dummy::node_id(),
                    kind,
                    span: dummy::span(),
                    attrs: dummy::attr_vec(),
                    tokens: None,
                };

                let args = vec![a, b];
                return rs::ExprKind::Call(P(func), args);
            }
        },
        BinOp::Unimplemented => {
            // TODO: annotate with a comment, eg. `ctx.annotete()`
            rs::BinOpKind::Add
        }
    };

    rs::ExprKind::Binary(
        Spanned {
            node: op,
            span: dummy::span(),
        },
        a,
        b,
    )
}

// TODO: could technically be rs::Call or rs::Methodcall, I think; unless all
// method calls in Python are "Attributes"
fn into_rs_call(
    function: &Box<py::Expression>,
    args: &[py::Expression],
    keywords: &Vec<py::Keyword>,
    ctx: &mut AstContext,
) -> rs::ExprKind {
    if keywords.len() != 0 {
        ctx.unimplemented_parameter("call", "kewords", keywords);
    }

    rs::ExprKind::Call(
        P(rs::Expr::from_py(function, ctx)),
        args.iter()
            .map(|arg| P(rs::Expr::from_py(arg, ctx)))
            .collect::<Vec<P<rs::Expr>>>(),
    )
}

/// Transpiler extension to the rustc_ast BinOp.
pub(crate) enum BinOp {
    /// Existing rustc_ast BinOp
    Ast(rs::BinOpKind),
    /// A method call
    Method(BinMethod),
    Unimplemented,
}

pub(crate) enum BinMethod {
    Powi,
}

enum UnOp {
    Ast(rs::UnOp),
    Identity,
    Inversion,
}

impl FromPy<py::UnaryOperator> for UnOp {
    fn from_py(op: &py::UnaryOperator, ctx: &mut AstContext) -> Self {
        match op {
            py::UnaryOperator::Pos => UnOp::Identity,
            py::UnaryOperator::Neg => UnOp::Ast(rs::UnOp::Neg),
            py::UnaryOperator::Not => UnOp::Ast(rs::UnOp::Not),
            py::UnaryOperator::Inv => UnOp::Inversion,
        }
    }
}

impl FromPy<py::Operator> for BinOp {
    fn from_py(op: &py::Operator, ctx: &mut AstContext) -> Self {
        match op {
            py::Operator::Add => BinOp::Ast(rs::BinOpKind::Add),
            py::Operator::Sub => BinOp::Ast(rs::BinOpKind::Sub),
            py::Operator::Mult => BinOp::Ast(rs::BinOpKind::Mul),
            // TODO: the transpiled probably needs a way to tag this and forward it to the
            // transpiled output This is definitely something that could be "user
            // transpilable"
            py::Operator::MatMult => {
                ctx.unimplemented_parameter("bin_op", "op", op);
                BinOp::Unimplemented
            }
            // TODO: Python 3 automatically uses floats for division, Rust should do that here too
            py::Operator::Div => {
                warn!("py::Div transpiled as rs::Div, this behaves incorrectly for integral arguments");
                BinOp::Ast(rs::BinOpKind::Div)
            }
            py::Operator::Mod => BinOp::Ast(rs::BinOpKind::Rem),
            // TODO: pow can be implemented using the standard library
            py::Operator::Pow => {
                // TODO: determine types and choose implementation based on runtime
                warn!("py::Pow transpiled as rs::powi, this only covers integral cases");
                BinOp::Method(BinMethod::Powi)
            }
            py::Operator::LShift => BinOp::Ast(rs::BinOpKind::Shl),
            py::Operator::RShift => BinOp::Ast(rs::BinOpKind::Shr),
            py::Operator::BitOr => BinOp::Ast(rs::BinOpKind::BitOr),
            py::Operator::BitXor => BinOp::Ast(rs::BinOpKind::BitXor),
            py::Operator::BitAnd => BinOp::Ast(rs::BinOpKind::BitAnd),
            // TODO: Python floor div is different from Rust integer division, this implementation
            // behaves incorrectly for some values
            py::Operator::FloorDiv => {
                warn!(
                    "py::FloorDiv transpiled as rs::Div, this behaves incorrectly for some values"
                );
                BinOp::Ast(rs::BinOpKind::Div)
            }
        }
    }
}

impl FromPy<py::Comparison> for BinOp {
    fn from_py(comp: &py::Comparison, ctx: &mut AstContext) -> Self {
        match comp {
            // TODO: The == operator compares the values of both the operands and checks for value
            // equality. Whereas is operator checks whether both the operands refer to the same
            // object or not.
            py::Comparison::Equal | py::Comparison::Is => {
                warn!("serpent does not differentiate between Python `==` and `is`");
                BinOp::Ast(rs::BinOpKind::Eq)
            }
            // TODO: The != operator compares the values of both the operands and checks for value
            // inequality. Whereas `is not` operator checks whether both the operands refer to the
            // same object or not.
            py::Comparison::NotEqual | py::Comparison::IsNot => {
                warn!("serpent does not differentiate between Python `!=` and `is not`");
                BinOp::Ast(rs::BinOpKind::Ne)
            }
            py::Comparison::Less => BinOp::Ast(rs::BinOpKind::Lt),
            py::Comparison::LessOrEqual => BinOp::Ast(rs::BinOpKind::Le),
            py::Comparison::Greater => BinOp::Ast(rs::BinOpKind::Gt),
            py::Comparison::GreaterOrEqual => BinOp::Ast(rs::BinOpKind::Ge),
            py::Comparison::In => {
                ctx.unimplemented_parameter("comparison", "comp", comp);
                BinOp::Unimplemented
            }
            py::Comparison::NotIn => {
                ctx.unimplemented_parameter("comparison", "comp", comp);
                BinOp::Unimplemented
            }
        }
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

impl FromPy<py::Number> for rs::Lit {
    fn from_py(number: &py::Number, ctx: &mut AstContext) -> Self {
        let kind = rs::LitKind::from_py(number, ctx);
        rs::Lit {
            token: dummy::token(kind.clone()),
            kind,
            span: dummy::span(),
        }
    }
}

impl FromPy<py::StringGroup> for rs::Lit {
    fn from_py(sg: &py::StringGroup, ctx: &mut AstContext) -> Self {
        let kind = rs::LitKind::from_py(sg, ctx);
        rs::Lit {
            token: dummy::token(kind.clone()),
            kind,
            span: dummy::span(),
        }
    }
}

impl FromPy<py::Number> for rs::LitKind {
    fn from_py(number: &py::Number, ctx: &mut AstContext) -> Self {
        match number {
            py::Number::Integer { value } => rs::LitKind::from_py(value, ctx),
            py::Number::Float { value } => rs::LitKind::from_py(value, ctx),
            py::Number::Complex { real, imag } => {
                ctx.unimplemented_parameter("number", "complex", number);
                rs::LitKind::Err(util::symbol(&format!("{:?}", number)))
            }
        }
    }
}

/// Converts a Pythonic float into a Rust float
impl FromPy<f64> for rs::LitKind {
    fn from_py(f: &f64, ctx: &mut AstContext) -> Self {
        let s = format!("{}", f);
        let sym = util::symbol(&s);
        // HACK: all floats unsuffixed; could use _f32, _f64 for clarity, or make it as
        // optional
        rs::LitKind::Float(sym, rs::LitFloatType::Unsuffixed)
    }
}

impl FromPy<num_bigint::BigInt> for rs::LitKind {
    fn from_py(bigint: &num_bigint::BigInt, ctx: &mut AstContext) -> Self {
        let (sign, data) = bigint.to_u32_digits();
        let value = {
            if data.len() == 1 {
                *data.first().unwrap()
            } else {
                // FIXME: handle ints larger than u32, maybe use bigint::u32_to_u128
                unimplemented!("integer literal is too large, not handled")
            }
        };
        // HACK: all ints unsuffixed; could use _i32, _u32 for clarity, or make it as
        // optional
        rs::LitKind::Int(value as u128, rs::LitIntType::Unsuffixed)
    }
}

impl FromPy<py::StringGroup> for rs::LitKind {
    fn from_py(sg: &py::StringGroup, ctx: &mut AstContext) -> Self {
        let s = match sg {
            py::StringGroup::Constant { value } => value,
            py::StringGroup::FormattedValue {
                value,
                conversion,
                spec,
            } => {
                ctx.unimplemented_parameter("StringGroup", "sg", sg);
                "()"
            }
            py::StringGroup::Joined { values } => {
                ctx.unimplemented_parameter("StringGroup", "sg", sg);
                return rs::LitKind::from_py(values.first().unwrap(), ctx);
            }
        };
        let sym = util::symbol(s);
        rs::LitKind::Str(sym, rs::StrStyle::Cooked)
    }
}
