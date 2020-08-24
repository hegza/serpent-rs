mod ast_to_ast;
mod codegen;
mod context;
mod parser_ext;
mod python;
mod rust;

use crate::error::SerpentError;
use ast_to_ast::TranspileNode;
use context::AstContext;
use itertools::Itertools;
use log::info;
use parser_ext::parse_comments;
use python::Node;
use rustpython_parser::parser as py_parser;

/// Represents successful transpile outcomes. Failures will be reported via
/// SerpentError.
pub struct TranspileOutput {
    /// Represents a Rust program made from a single file of Python.
    pub program: String,
}

/// Transpiles given Python source code to Rust source code. No information of
/// other modules is necessary, because Python packages are self-contained.
/// # Arguments
///
/// * `infer_main` - Whether to create a runnable main function from
///   free-standing Python code.
pub fn transpile_python(src: &str, infer_main: bool) -> Result<TranspileOutput, SerpentError> {
    // Parse Python source into a Python AST using RustPython
    info!("Parsing Python program");
    let py_ast = py_parser::parse_program(src)?;
    let stmt_nodes = py_ast
        .statements
        .into_iter()
        .map(|stmt| python::NodeKind::from(stmt));

    // Parse comments with their locations
    let comments = parse_comments(src);
    let comment_nodes = comments
        .into_iter()
        .map(|comment| python::NodeKind::Comment(comment));

    // Re-arrange comments and statements
    let py_nodes = stmt_nodes
        .merge_by(comment_nodes, |a, b| {
            let (a_loc, b_loc) = (a.location(), b.location());

            if a_loc.row() < b_loc.row() {
                true
            } else if a_loc.row() == b_loc.row() {
                a_loc.column() < b_loc.column()
            } else {
                false
            }
        })
        .collect::<Vec<python::NodeKind>>();

    // The Python program is a sequence of statements, comments, and newlines
    // which can be translated to Rust
    let mut context = AstContext::new(infer_main, &py_nodes);
    for py_node in &py_nodes {
        py_node
            .transpile(&mut context)
            .map_err(|inner| inner.with_source(src, None))?;
    }

    let rust_ast = context
        .finish()
        .map_err(|inner| inner.with_source(&src, None))?;

    let out_str = codegen::fidelity_print(&rust_ast);

    Ok(TranspileOutput { program: out_str })
}
