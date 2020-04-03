use derive_deref::Deref;
use log::{debug, trace};
use num_bigint::BigInt;
use rustpython_parser::ast::*;
use syn as rs_ast;

pub fn visit_statement(stmt: Statement) -> rs_ast::Stmt {
    let _location = stmt.location;
    let stmt = stmt.node;
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
        } => visit_function_def(is_async, name, args, body, decorator_list, returns),
        Return { value } => syn::Stmt::Expr(visit_return(value)),
        _ => {
            println!("unimplemented: {:?}", stmt);
            unimplemented!()
        }
    }
}

fn visit_return(value: Option<Expression>) -> syn::Expr {
    let expr_return = syn::ExprReturn {
        attrs: vec![],
        return_token: <syn::Token![return]>::default(),
        expr: value.and_then(|e| Some(Box::new(visit_expression(&e)))),
    };
    syn::Expr::Return(expr_return)
}

fn visit_function_def(
    is_async: bool,
    name: String,
    args: Box<Parameters>,
    body: Suite,
    decorator_list: Vec<Expression>,
    returns: Option<Expression>,
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
        inputs: visit_params(*args),
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

fn visit_params(args: Parameters) -> syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma> {
    // HACK: drop everything but the ordinary parameters for now
    let args = args.args;

    let mut list = syn::punctuated::Punctuated::new();
    for arg in args {
        let rust_param = visit_param(arg);
        list.push(rust_param);
    }
    list
}

fn visit_param(arg: Parameter) -> syn::FnArg {
    let _location = arg.location;
    let id_pat = Pat::from(arg.arg).0;
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

impl From<String> for Pat {
    fn from(s: String) -> Self {
        Pat(syn::Pat::Ident(syn::PatIdent {
            attrs: vec![],
            by_ref: None,
            mutability: None,
            ident: Ident::from(s).0,
            subpat: None,
        }))
    }
}

fn visit_body(body: Suite) -> syn::Block {
    let stmts = body
        .into_iter()
        .map(|py_stmt| visit_statement(py_stmt))
        .collect::<Vec<syn::Stmt>>();
    syn::Block {
        brace_token: syn::token::Brace(proc_macro2::Span::call_site()),
        stmts,
    }
}

fn visit_assign(targets: &[Expression], value: &Expression) -> rs_ast::Stmt {
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
        rs_ast::Pat::Ident(rs_ast::PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident,
            subpat: None,
        })
    } else {
        unimplemented!()
    };
    rs_ast::Pat::Wild(syn::PatWild {
        attrs: Vec::new(),
        underscore_token: <rs_ast::Token![_]>::default(),
    });

    // TODO: Python 'assign' could be a Local or an assignment expression in Rust
    // HACK: Used Local for now
    let local = rs_ast::Local {
        attrs: Vec::new(),
        let_token: <rs_ast::Token![let]>::default(),
        pat: lhs_target,
        init: Some((
            <rs_ast::Token![=]>::default(),
            Box::new(visit_expression(value)),
        )),
        semi_token: <rs_ast::Token![;]>::default(),
    };
    let local = rs_ast::Stmt::Local(local);
    debug!("Assign({:?}, {:?}) -> {:?}", &targets, &value, &local);
    local
}

fn visit_expression(expr: &Expression) -> rs_ast::Expr {
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
        ExpressionType::Call {
            function,
            args,
            keywords,
        } => syn::Expr::Call(syn::ExprCall {
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

fn visit_number(number: &Number) -> rs_ast::Expr {
    trace!("visit: {:?}", number);
    use Number::*;
    let rs_number = match number {
        Integer { value } => visit_bigint(&value),
        _ => unimplemented!(),
    };
    debug!("{:?} -> {:?}", &number, rs_number);
    rs_number
}

fn visit_bigint(bigint: &BigInt) -> rs_ast::Expr {
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
            rs_ast::Expr::Lit(rs_ast::ExprLit {
                attrs: Vec::new(),
                lit: rs_ast::Lit::new(literal),
            })
        }
        _ => unimplemented!(),
    };
    debug!("{:?} -> {:?}", bigint, expr);
    expr
}
