// TODO: turn Visit create into a macro
// TODO: turn visit() into a macro
use super::Result;
use super::*;

use proc_macro2::Span;
use rustpython_parser::ast;
use syn;

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
            _ => return Err(TranspileError::unimplemented(self.0, None)),
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
                return Err(TranspileError::unimplemented(
                    &(value, conversion, spec),
                    None,
                ))
            }
            ast::StringGroup::Joined { values } => {
                return Err(TranspileError::unimplemented(values, None))
            }
        })
    }
}
