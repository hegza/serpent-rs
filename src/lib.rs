use derive_deref::Deref;
use parser::parse_program;
use quote::ToTokens;
use rustpython_parser::ast::*;
use rustpython_parser::parser;
use syn as rs_ast;

/// Main entry point
pub fn transpile(python_source: &str) -> String {
    let python_program_ast = parse_program(python_source)
        .expect("source string cannot be made into a Python abstract syntax tree (AST).");

    let rust_program: Vec<RsStmt> = python_program_ast
        .statements
        .into_iter()
        .map(|py_stmt| py_stmt.into())
        .collect();

    rust_program
        .iter()
        .map(|stmt| {
            let stream = stmt.0.to_token_stream();
            println!("token: {:?}", &stream);
            stream.to_string()
        })
        .fold(String::new(), |acc, next| acc + &next)
}

#[derive(Deref, Clone)]
struct RsStmt(pub rs_ast::Stmt);

impl From<Statement> for RsStmt {
    fn from(py_stmt: Statement) -> Self {
        let rs_stmt = transpiler::visit_statement(py_stmt);
        RsStmt(rs_stmt)
    }
}

mod transpiler {
    use num_bigint::BigInt;
    use rustpython_parser::ast::*;
    use syn as rs_ast;
    use log::{trace, debug};

    pub fn visit_statement(stmt: Statement) -> rs_ast::Stmt {
        let location = stmt.location;
        let stmt = stmt.node;
        trace!("visit: {:?}", stmt);

        use StatementType::*;
        match stmt {
            Assign { targets, value } => visit_assign(&targets, &value),
            _ => unimplemented!(),
        }
    }

    fn visit_assign(targets: &[Expression], value: &Expression) -> rs_ast::Stmt {
        trace!("visit: Assign({:?}, {:?})", targets, value);
        let lhs_target = if targets.len() == 1 {
            let target = targets.first().unwrap();
            let location = &target.location;
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
        let location = &expr.location;
        let expr = &expr.node;

        use ExpressionType::*;
        let rs_expr = match expr {
            Number { value } => visit_number(&value),
            _ => unimplemented!(),
        };
        debug!("{:?} -> {:?}", &expr, &rs_expr);
        rs_expr
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
}
