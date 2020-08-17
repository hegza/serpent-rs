// TODO: turn Visit create into a macro
// TODO: turn visit() into a macro
use super::*;

use ast::Location;
use proc_macro2::Span;
use rustpython_parser::ast;
use syn;

/// A type alias for `Result<T, TranspileNodeError>`.
pub type Result<T> = result::Result<T, TranspileNodeError>;

pub trait Visit {
    type Output;

    fn visit(&self) -> Result<Self::Output>;
}

#[derive(Debug)]
pub struct BinOp<'a> {
    pub left: &'a Box<ast::Expression>,
    pub op: &'a ast::Operator,
    pub right: &'a Box<ast::Expression>,
}

impl<'a> Visit for BinOp<'a> {
    type Output = syn::Expr;

    fn visit(&self) -> Result<syn::Expr> {
        let bin_expr = syn::ExprBinary {
            attrs: vec![],
            left: Box::new(Expression(&self.left).visit()?),
            op: Operator(&self.op).visit()?,
            right: Box::new(Expression(&self.right).visit()?),
        };
        Ok(syn::Expr::Binary(bin_expr))
    }
}

#[derive(Debug)]
pub struct Operator<'a>(pub &'a ast::Operator);

impl<'a> Visit for Operator<'a> {
    type Output = syn::BinOp;

    fn visit(&self) -> Result<Self::Output> {
        Ok(match self.0 {
            ast::Operator::Add => syn::BinOp::Add(<syn::Token![+]>::default()),
            _ => return Err(TranspileNodeError::unimplemented(self.0, None)),
        })
    }
}

#[derive(Debug)]
pub struct Parameter<'a>(pub &'a ast::Parameter);

impl<'a> Visit for Parameter<'a> {
    type Output = syn::FnArg;

    fn visit(&self) -> Result<syn::FnArg> {
        let _loc = &self.0.location;
        let id_pat = Pat::from(&self.0.arg).0;
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
        Ok(syn::FnArg::Typed(pat))
    }
}

#[derive(Debug)]
pub struct Expression<'a>(pub &'a ast::Expression);

impl<'a> Visit for Expression<'a> {
    type Output = syn::Expr;

    fn visit(&self) -> Result<syn::Expr> {
        trace!("Expression::visit -> {:?}", self);
        let location = &self.0.location;
        let expr = &self.0.node;

        let rs_expr = match expr {
            ast::ExpressionType::Number { value } => visit_number(&value)?,
            ast::ExpressionType::Binop { a, op, b } => BinOp {
                left: a,
                op,
                right: b,
            }
            .visit()?,
            ast::ExpressionType::Identifier { name } => syn::Expr::Path(Path(name).visit()?),
            ast::ExpressionType::Call { function, args, .. } => syn::Expr::Call(syn::ExprCall {
                attrs: vec![],
                func: Box::new(Expression(&*function).visit()?),
                paren_token: syn::token::Paren(proc_macro2::Span::call_site()),
                args: visit_args(args)?,
            }),
            ast::ExpressionType::String { value } => StringGroup(value).visit()?,
            _ => {
                println!("unimplemented: {:?}\nlocation: {:?}", expr, location);
                unimplemented!()
            }
        };
        debug!("{:?} -> {:?}", &self, &rs_expr);
        Ok(rs_expr)
    }
}

pub struct Path<'a>(pub &'a str);

impl<'a> Visit for Path<'a> {
    type Output = syn::ExprPath;

    fn visit(&self) -> Result<syn::ExprPath> {
        let name = self.0;

        Ok(syn::ExprPath {
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
        })
    }
}

#[derive(Debug)]
pub struct StringGroup<'a>(pub &'a ast::StringGroup);

impl<'a> Visit for StringGroup<'a> {
    type Output = syn::Expr;

    fn visit(&self) -> Result<syn::Expr> {
        Ok(match self.0 {
            ast::StringGroup::Constant { value } => syn::Expr::Lit(syn::ExprLit {
                attrs: vec![],
                lit: syn::Lit::Str(syn::LitStr::new(value, Span::call_site())),
            }),
            ast::StringGroup::FormattedValue {
                value,
                conversion,
                spec,
            } => {
                return Err(TranspileNodeError::unimplemented(
                    &(value, conversion, spec),
                    None,
                ))
            }
            ast::StringGroup::Joined { values } => {
                return Err(TranspileNodeError::unimplemented(values, None))
            }
        })
    }
}

// Old implementations

/// Converts a Python AST node into a Rust AST node.
///
/// Note that if the parameter is an expression statement, a `Stmt::Semi` is
/// returned instead of an `Stmt::Expr`. This is the correct functionality.
pub(crate) fn visit_statement(
    stmt: &ast::StatementType,
    location: Option<&Location>,
    cursor: Option<&Cursor<PyNode>>,
) -> result::Result<syn::Stmt, TranspileNodeError> {
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
            cursor,
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
    cursor: Option<&Cursor<PyNode>>,
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
    let block = visit_body(body, cursor)?; // { ... }
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
fn visit_body(body: &ast::Suite, cursor: Option<&Cursor<PyNode>>) -> Result<syn::Block> {
    let stmts = body
        .into_iter()
        .map(|py_stmt| visit_statement(&py_stmt.node, Some(&py_stmt.location), cursor))
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
        _ => return Err(TranspileNodeError::unimplemented(bigint, None)),
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
