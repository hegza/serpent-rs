mod transpiler;

use derive_deref::Deref;
use parser::parse_program;
use quote::ToTokens;
use rustpython_parser::ast::*;
use rustpython_parser::parser;
use syn;

/// Transpiles given Python source code to Rust
pub fn transpile(python_source: &str) -> String {
    // Parse Python into a program described by an AST
    let py_program =
        parse_program(python_source).expect("source string cannot be made into a Python AST.");

    // Map Python into Rust, statement by statement
    let rs_statements: Vec<RsStmt> = py_program
        .statements
        .into_iter()
        .map(|py_stmt| py_stmt.into())
        .collect();

    // Format Rust statements
    let formatted = rs_statements
        .iter()
        .map(|stmt| {
            let tokens = stmt.to_token_stream();

            // HACK: Create a mock 'main' item to get rustfmt to accept it
            let mock_source = "fn mock() {".to_owned() + &tokens.to_string() + "}";

            // Format using rustfmt
            let (_, file_map, _) = rustfmt::format_input::<Vec<u8>>(
                rustfmt::Input::Text(mock_source),
                &rustfmt::config::Config::default(),
                None,
            )
            .unwrap();

            let output = &file_map.first().unwrap().1;
            let output_str = output.chars().map(|(c, _)| c).collect::<String>();

            // Take all but the first and last line from the rustfmt output
            let mut relevant = output_str
                .lines()
                .skip(1)
                .map(|x| x.trim().to_owned())
                .collect::<Vec<String>>();
            relevant.pop();

            relevant
        })
        .flatten();

    // Catenate statements with newlines and return
    formatted.fold(String::new(), |acc, next| acc + &next + "\n")
}

#[derive(Clone, Deref)]
struct RsStmt(pub syn::Stmt);

impl From<Statement> for RsStmt {
    fn from(py_stmt: Statement) -> Self {
        let rs_stmt = transpiler::visit_statement(py_stmt);
        RsStmt(rs_stmt)
    }
}
