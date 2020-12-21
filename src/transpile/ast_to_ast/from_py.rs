//! Defines conversions from Python AST nodes into Rust AST nodes.
//!
//! TODO: Review all HACKs, FIXMEs

use super::{dummy, util};
use crate::transpile::context::AstContext;
use itertools::Itertools;
use log::{error, warn};
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ap_rustc_span::source_map::Spanned;
use rustc_ap_rustc_span::symbol;
use rustc_ast::{ast as rs, ptr::P};
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
            py::ExpressionType::BoolOp { op: _, values: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::Binop { a, op, b } => {
                return to_rs_bin_op(BinOp::from_py(op, ctx), a, b, ctx)
            }
            // Python subscript `a[b]` converts to Rust Index `a[b]`.
            py::ExpressionType::Subscript { a, b } => {
                let path = rs::Expr::from_py(a, ctx);
                let index = rs::Expr::from_py(b, ctx);

                return rs::ExprKind::Index(P(path), P(index));
            }
            py::ExpressionType::Unop { op, a } => {
                return to_rs_un_op(UnOp::from_py(op, ctx), a, ctx)
            }
            py::ExpressionType::Await { value: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::Yield { value: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::YieldFrom { value: _ } => ctx.unimplemented_item(expr),
            // A Python chained comparison, eg. `a < b` or `a < b < c`
            // TODO: `a < b && b < c`
            py::ExpressionType::Compare { vals, ops } => {
                if vals.len() != 2 || ops.len() != 1 {
                    ctx.unimplemented_parameter("chained_comparison", "(vals, ops)", &(vals, ops));
                }
                let op = ops.first().unwrap();
                let (a, b) = (&vals[0], &vals[1]);
                return to_rs_bin_op(BinOp::from_py(op, ctx), a, b, ctx);
            }
            // An attribute maps to field access
            py::ExpressionType::Attribute { value, name } => {
                // Check for a remapping on the attribute
                let (object, field) = ctx.get_remapped_attribute(&value.node, name).unwrap_or((
                    rs::Expr::from_py(value, ctx),
                    symbol::Ident::from_py(name, ctx),
                ));

                return rs::ExprKind::Field(P(object), field);
            }
            py::ExpressionType::Call {
                function,
                args,
                keywords,
            } => {
                ctx.start_call();
                let rs_call = to_rs_call(function, args, keywords, ctx);
                ctx.end_call();
                return rs_call;
            }
            py::ExpressionType::Number { value } => {
                return rs::ExprKind::Lit(rs::Lit::from_py(value, ctx))
            }
            py::ExpressionType::List { elements } =>
            // A `vec![]` expression is a sensible default for a list expression
            {
                return to_rs_array(elements, ctx)
            }
            py::ExpressionType::Tuple { elements } => {
                return rs::ExprKind::Tup(
                    elements
                        .iter()
                        .map(|e| P(rs::Expr::from_py(e, ctx)))
                        .collect::<Vec<P<rs::Expr>>>(),
                )
            }
            py::ExpressionType::Dict { elements: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::Set { elements: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::Comprehension {
                kind: _,
                generators: _,
            } => ctx.unimplemented_item(expr),
            py::ExpressionType::Starred { value: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::Slice { elements } => {
                if elements[2].node != py::ExpressionType::None {
                    unimplemented!("step argument in slice is not supported: {:?}", expr);
                }
                let start = rs::Expr::from_py(&elements[0], ctx);
                let end = rs::Expr::from_py(&elements[1], ctx);
                // NOTE: use half-open range by default
                // TODO: figure out if a closed range can be specified in Python, and use that
                // here then
                return rs::ExprKind::Range(
                    Some(P(start)),
                    Some(P(end)),
                    rs::RangeLimits::HalfOpen,
                );
            }
            py::ExpressionType::String { value } => {
                return rs::ExprKind::Lit(rs::Lit::from_py(value, ctx))
            }
            py::ExpressionType::Bytes { value: _ } => ctx.unimplemented_item(expr),
            // An identifier in an expression is probably a rs::Path
            py::ExpressionType::Identifier { name } => return id_to_path(name, ctx),
            py::ExpressionType::Lambda { args: _, body: _ } => ctx.unimplemented_item(expr),
            py::ExpressionType::IfExpression {
                test: _,
                body: _,
                orelse: _,
            } => ctx.unimplemented_item(expr),
            py::ExpressionType::NamedExpression { left: _, right: _ } => {
                ctx.unimplemented_item(expr)
            }
            py::ExpressionType::True => ctx.unimplemented_item(expr),
            py::ExpressionType::False => ctx.unimplemented_item(expr),
            py::ExpressionType::None => {
                return rs::ExprKind::Path(None, util::str_to_path("std::option::Option::None"));
            }
            py::ExpressionType::Ellipsis => ctx.unimplemented_item(expr),
        };

        // HACK: Just return a placeholder expression for now
        let block = rs::Block {
            stmts: vec![],
            id: dummy::node_id(),
            rules: rs::BlockCheckMode::Default,
            span: dummy::span(),
            tokens: None,
        };
        rs::ExprKind::Block(P(block), None)
    }
}

impl FromPy<String> for symbol::Ident {
    fn from_py(name: &String, ctx: &mut AstContext) -> Self {
        util::ident(name)
    }
}

fn to_rs_array(elements: &[py::Expression], ctx: &mut AstContext) -> rs::ExprKind {
    rs::ExprKind::Array(
        elements
            .iter()
            .map(|expr| P(rs::Expr::from_py(expr, ctx)))
            .collect::<Vec<P<rs::Expr>>>(),
    )
}

// FIXME: can't generate rs_vec_macro, because tokens are not available
/*
fn to_rs_vec_macro(elements: &[py::Expression], ctx: &mut AstContext) -> rs::MacCall {
    let args = elements
        .iter()
        .map(|expr| {
            let rs_expr = rs::ExprKind::from_py(expr, ctx);
        })
        .collect::<Vec<rs::ExprKind>>();
    let args = rs::MacArgs::Delimited(
        rustc_ast::tokenstream::DelimSpan::dummy(),
        rs::MacDelimiter::Bracket,
        args,
    );
    rs::MacCall {
        path: "vec",
        args,
        prior_type_ascription: None,
    }
}
*/

impl FromPy<py::Expression> for rs::PatKind {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        match &expr.node {
            py::ExpressionType::BoolOp { op: _, values: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Binop { a: _, op: _, b: _ } => ctx.unimplemented_pat(expr),
            // A subscript as a pattern does not exist in Rust; this requires another solution
            py::ExpressionType::Subscript { a: _, b: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Unop { op: _, a: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Await { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Yield { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::YieldFrom { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Compare { vals: _, ops: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Attribute { value: _, name: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Call {
                function: _,
                args: _,
                keywords: _,
            } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Number { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::List { elements: _ } => ctx.unimplemented_pat(expr),
            // Assignment to tuple in Python becomes an assignment to tuple in Rust
            py::ExpressionType::Tuple { elements } => rs::PatKind::Tuple(elements.iter().map(|e| rs::Pat::from_py(e, ctx)).map(P).collect::<Vec<P<rs::Pat>>>()),
            py::ExpressionType::Dict { elements: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Set { elements: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Comprehension { kind: _, generators: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Starred { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Slice { elements: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::String { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Bytes { value: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::Identifier { name } => util::str_to_pat_kind( name ),
            py::ExpressionType::Lambda { args: _, body: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::IfExpression { test: _, body: _, orelse: _ } => ctx.unimplemented_pat(expr),
            py::ExpressionType::NamedExpression { left: _, right: _ } => ctx.unimplemented_pat(expr),
            // The following Python expressions can not be Rust patterns
            py::ExpressionType::True | py::ExpressionType::False | py::ExpressionType::None | py::ExpressionType::Ellipsis => panic!(
                "{:?} cannot be made into a Rust pattern, it's also likely invalid Python code unless transpiler implementation has a mistake", expr
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
            tokens: None,
        }
    }
}

fn id_to_path(id: &str, _ctx: &mut AstContext) -> rs::ExprKind {
    let path = util::str_to_path(id);

    // FIXME: Paths are never qualified (first arg = None)
    rs::ExprKind::Path(None, path)
}

fn to_rs_un_op(kind: UnOp, a: &Box<py::Expression>, ctx: &mut AstContext) -> rs::ExprKind {
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
                token: util::ast_lit_into_token_lit(one_kind.clone()),
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

fn to_rs_bin_op(
    kind: BinOp,
    a: &py::Expression,
    b: &py::Expression,
    ctx: &mut AstContext,
) -> rs::ExprKind {
    let a = P(rs::Expr::from_py(a, ctx));
    let b = P(rs::Expr::from_py(b, ctx));

    let op = match kind {
        BinOp::Native(op) => op,
        BinOp::Method(method) => match method {
            StdMethod::Powi => {
                // HACK: just throwing in a `std::f64::powi` here without thinking about it too
                // much
                let kind = id_to_path("f64::powi", ctx);
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
            // TODO: annotate with a comment, eg. `ctx.annotate()`
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

/// Returns an `rs::MethodCall` if the source function is an attribute,
/// `rs::Call` otherwise.
fn to_rs_call(
    function: &Box<py::Expression>,
    args: &[py::Expression],
    kw_args: &Vec<py::Keyword>,
    ctx: &mut AstContext,
) -> rs::ExprKind {
    warn!("Mapping call: {:?}", function);

    // HACK: just append keyword arguments as ordinary arguments
    if kw_args.len() != 0 {
        warn!(
            "Mapping keyword arguments to ordinary arguments, keyword arguments: {:?}",
            args
        );
    }
    let args = args
        .iter()
        .cloned()
        .chain(kw_args.iter().map(|keyword| keyword.value.clone()))
        .collect::<Vec<py::Expression>>();

    // Transpile args
    let mut args: Vec<P<rs::Expr>> = args
        .iter()
        .map(|arg| P(rs::Expr::from_py(arg, ctx)))
        .collect();

    // Check if this function is remapped
    if let Some(template) = ctx.remap_function(&function.node) {
        let mut ret = None;

        for template_str in &template {
            // TODO: better error handling
            let mut iter = template_str.split_whitespace();
            let first_word = iter.nth(0).unwrap();
            let template_rest = iter.join(" ");

            match first_word {
                "push_parameter" => {
                    let mut iter = template_rest.split_whitespace();
                    let first_word = iter.nth(0).unwrap();

                    match first_word {
                        "call" => {
                            let template = iter.join(" ");
                            let push_expr = dummy::expr(template_to_path(&template));

                            match ret {
                                Some(ref mut ret) => {
                                    if let rs::ExprKind::Call(_func, args) = ret {
                                        args.push(P(push_expr));
                                    } else {
                                        panic!()
                                    }
                                }
                                None => args.push(P(push_expr)),
                            }
                        }
                        first_word => {
                            let template = std::iter::once(first_word).chain(iter).join(" ");
                            let push_expr = dummy::expr(template_to_path(&template));

                            match ret {
                                Some(ref mut ret) => {
                                    if let rs::ExprKind::Call(_func, args) = ret {
                                        args.push(P(push_expr));
                                    } else {
                                        panic!()
                                    }
                                }
                                None => args.push(P(push_expr)),
                            }
                        }
                    }
                }
                "call" => ret = Some(template_to_call(&template_rest, args.clone())),
                "macro_call" => {
                    error!("macro_call skipped");
                    continue;
                }
                x => panic!("unknown first word: {:?}", x),
            }
        }

        ret.unwrap_or(rs::ExprKind::Path(None, util::str_to_path("")))
    }
    // Attribute access in the form of `value.name`
    else if let py::ExpressionType::Attribute { value, name } = &function.node {
        // The expression is a method call

        // Convert `value` into the method call receiver
        let receiver = rs::Expr::from_py(value, ctx);

        // Convert `name` into the function path
        let seg = util::str_to_path_seg(name);

        let receiver_and_args = vec![P(receiver)]
            .into_iter()
            .chain(args)
            .collect::<Vec<P<rs::Expr>>>();

        // The first element of the vector of an Expr is the expression that evaluates
        // to the object on which the method is being called on (the receiver), and the
        // remaining elements are the rest of the arguments.
        rs::ExprKind::MethodCall(seg, receiver_and_args, dummy::span())
    } else {
        // The expression is not a method call

        let function = rs::Expr::from_py(function, ctx);

        rs::ExprKind::Call(P(function), args)
    }
}

fn template_to_call(template: &str, args: Vec<P<rs::Expr>>) -> rs::ExprKind {
    // HACK: we just map everything to calls, ignoring any possibility of
    // method_call
    let path = rs::Path::from_ident(util::ident(&template));
    let func = rs::ExprKind::Path(None, path);
    let func: rs::Expr = rs::Expr {
        id: dummy::node_id(),
        kind: func,
        span: dummy::span(),
        attrs: dummy::attr_vec(),
        tokens: None,
    };
    rs::ExprKind::Call(P(func), args)
}

fn template_to_path(template: &str) -> rs::ExprKind {
    let path = rs::Path::from_ident(util::ident(&template));
    let call = rs::ExprKind::Path(None, path);
    call
}

/// Represents a binary operation in Rust. Extends rs::BinOp.
enum BinOp {
    /// A native rs::BinOp represented by a symbol, eg. '>'.
    Native(rs::BinOpKind),
    /// A call to a method with the receiver being left-hand operand, and the
    /// parameter being... the parameter.
    Method(StdMethod),
    Unimplemented,
}

enum StdMethod {
    /// Represents `{}::powi(a, b)`.
    Powi,
}

enum UnOp {
    Ast(rs::UnOp),
    Identity,
    Inversion,
}

impl FromPy<py::UnaryOperator> for UnOp {
    fn from_py(op: &py::UnaryOperator, _ctx: &mut AstContext) -> Self {
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
            py::Operator::Add => BinOp::Native(rs::BinOpKind::Add),
            py::Operator::Sub => BinOp::Native(rs::BinOpKind::Sub),
            py::Operator::Mult => BinOp::Native(rs::BinOpKind::Mul),
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
                BinOp::Native(rs::BinOpKind::Div)
            }
            py::Operator::Mod => BinOp::Native(rs::BinOpKind::Rem),
            // TODO: pow can be implemented using the standard library
            py::Operator::Pow => {
                // TODO: determine types and choose implementation based on runtime
                warn!("py::Pow transpiled as rs::powi, this only covers integral cases");
                BinOp::Method(StdMethod::Powi)
            }
            py::Operator::LShift => BinOp::Native(rs::BinOpKind::Shl),
            py::Operator::RShift => BinOp::Native(rs::BinOpKind::Shr),
            py::Operator::BitOr => BinOp::Native(rs::BinOpKind::BitOr),
            py::Operator::BitXor => BinOp::Native(rs::BinOpKind::BitXor),
            py::Operator::BitAnd => BinOp::Native(rs::BinOpKind::BitAnd),
            // TODO: Python floor div is different from Rust integer division, this implementation
            // behaves incorrectly for some values
            py::Operator::FloorDiv => {
                warn!(
                    "py::FloorDiv transpiled as rs::Div, this behaves incorrectly for some values"
                );
                BinOp::Native(rs::BinOpKind::Div)
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
                BinOp::Native(rs::BinOpKind::Eq)
            }
            // TODO: The != operator compares the values of both the operands and checks for value
            // inequality. Whereas `is not` operator checks whether both the operands refer to the
            // same object or not.
            py::Comparison::NotEqual | py::Comparison::IsNot => {
                warn!("serpent does not differentiate between Python `!=` and `is not`");
                BinOp::Native(rs::BinOpKind::Ne)
            }
            py::Comparison::Less => BinOp::Native(rs::BinOpKind::Lt),
            py::Comparison::LessOrEqual => BinOp::Native(rs::BinOpKind::Le),
            py::Comparison::Greater => BinOp::Native(rs::BinOpKind::Gt),
            py::Comparison::GreaterOrEqual => BinOp::Native(rs::BinOpKind::Ge),
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
            token: util::ast_lit_into_token_lit(kind.clone()),
            kind,
            span: dummy::span(),
        }
    }
}

impl FromPy<py::StringGroup> for rs::Lit {
    fn from_py(sg: &py::StringGroup, ctx: &mut AstContext) -> Self {
        let kind = rs::LitKind::from_py(sg, ctx);
        rs::Lit {
            token: util::ast_lit_into_token_lit(kind.clone()),
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
            py::Number::Complex { real: _, imag: _ } => {
                ctx.unimplemented_parameter("number", "complex", number);
                rs::LitKind::Err(util::symbol(&format!("{:?}", number)))
            }
        }
    }
}

/// Converts a Pythonic float into a Rust float
impl FromPy<f64> for rs::LitKind {
    fn from_py(f: &f64, _ctx: &mut AstContext) -> Self {
        let s = format!("{}", f);
        let sym = util::symbol(&s);
        // HACK: all floats unsuffixed; could use _f32, _f64 for clarity, or make it as
        // optional
        rs::LitKind::Float(sym, rs::LitFloatType::Unsuffixed)
    }
}

impl FromPy<num_bigint::BigInt> for rs::LitKind {
    fn from_py(bigint: &num_bigint::BigInt, _ctx: &mut AstContext) -> Self {
        // HACK: workaround bigints via string conversion
        let value: u128 = format!("{}", bigint).parse().unwrap();

        // TODO: see if this covers both positive and negative cases right; it does if
        // negatives are implemented as unary instead of negative bigint literals

        // HACK: all ints unsuffixed; could use _i32, _u32 for clarity, or make it as
        // optional
        rs::LitKind::Int(value, rs::LitIntType::Unsuffixed)
    }
}

impl FromPy<py::StringGroup> for rs::LitKind {
    fn from_py(sg: &py::StringGroup, ctx: &mut AstContext) -> Self {
        let s = match sg {
            py::StringGroup::Constant { value } => value,
            py::StringGroup::FormattedValue {
                value: _,
                conversion: _,
                spec: _,
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

impl FromPy<py::Expression> for rs::TyKind {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        rs::TyKind::from_py(&expr.node, ctx)
    }
}

impl FromPy<py::ExpressionType> for rs::TyKind {
    fn from_py(expr: &py::ExpressionType, ctx: &mut AstContext) -> Self {
        if let py::ExpressionType::Identifier { name } = expr {
            match name.as_ref() {
                "int" => rs::TyKind::Path(None, util::str_to_path("i64")),
                // TODO: list out all known Python types
                _ => {
                    ctx.unimplemented_parameter("expression type", "name", name);
                    rs::TyKind::Err
                }
            }
        } else {
            ctx.unimplemented_parameter("expression type", "expr", expr);
            rs::TyKind::Err
        }
    }
}

impl FromPy<py::Expression> for rs::FnRetTy {
    fn from_py(expr: &py::Expression, ctx: &mut AstContext) -> Self {
        rs::FnRetTy::from_py(&expr.node, ctx)
    }
}

impl FromPy<py::ExpressionType> for rs::FnRetTy {
    fn from_py(expr: &py::ExpressionType, ctx: &mut AstContext) -> Self {
        // Forwards to rs::TyKind
        let kind = rs::TyKind::from_py(expr, ctx);
        let ty = rs::Ty {
            id: dummy::node_id(),
            kind,
            span: dummy::span(),
            tokens: None,
        };
        rs::FnRetTy::Ty(P(ty))
    }
}
