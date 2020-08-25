mod ast_to_ast;
mod codegen;
mod config;
mod context;
mod parser_ext;
mod python;
mod rust;

use crate::{error::SerpentError, PyModule};
use ast_to_ast::TranspileNode;
use config::TranspileConfig;
use context::{AstContext, PrintContext, ProgramContext};
use itertools::Itertools;
use log::info;
use parser_ext::parse_comments;
use python::Node;
use rustc_ap_rustc_span::with_default_session_globals;
use rustpython_parser::{location::Location, parser as py_parser};
use std::{fs, path};

/// Transpiles given Python source code to Rust source code assuming no other
/// module context.
///
/// # Arguments
///
/// * `infer_main` - Whether to create a runnable main function from
///   free-standing Python code.
pub fn transpile_str(src: &str, infer_main: bool) -> Result<String, SerpentError> {
    let py_nodes = parse_str_to_py_ast(src)?;
    let cfg = TranspileConfig::default();

    // The Python program is a sequence of statements, comments, and newlines
    // which can be translated to Rust
    let dummy = vec![];
    let mut context = AstContext::new(&dummy, &py_nodes, &cfg);
    for py_node in &py_nodes {
        py_node
            .transpile(&mut context)
            .map_err(|inner| inner.with_source(src, None))?;
    }

    let rust_ast = context
        .finish()
        .map_err(|inner| inner.with_source(&src, None))?;

    let out_str = codegen::fidelity_print(&rust_ast, &cfg)?;

    Ok(out_str)
}

/// Transpiles a module from the given directory to Rust.
pub fn transpile_module_dir(
    dir_path: impl AsRef<path::Path>,
) -> Result<Vec<(path::PathBuf, String)>, SerpentError> {
    let py_module = PyModule::from_dir_path(dir_path)?;
    let files = py_module.files();

    // Transpile files one-by-one
    let mut prog_ctx = ProgramContext::new(&py_module);
    let mut transpiled_sources = Vec::with_capacity(files.len());
    for path in &files {
        // Transpile this file
        let transpiled = transpile_file(path, &mut prog_ctx)?;
        transpiled_sources.push((path.clone(), transpiled));

        // Next file
        prog_ctx.advance();
    }

    Ok(transpiled_sources)
}

fn transpile_file(path: &path::PathBuf, ctx: &mut ProgramContext) -> Result<String, SerpentError> {
    info!("Parsing {:?} into Python AST", path);

    // Parse file into an AST
    let content =
        normalize_line_endings::normalized(fs::read_to_string(path)?.chars()).collect::<String>();
    let ast = parse_str_to_py_ast(&content)?;

    // Resolve relative mod symbols so that AST context can figure out which modules
    // are local and which are foreign
    let relative_mod_symbols = ctx.source_module().resolve_mod_symbols_relative_to(path);

    // Transform the Python AST into a Rust AST
    let cfg = TranspileConfig::default();
    let out_str: Result<String, SerpentError> = with_default_session_globals(|| {
        let mut ast_ctx = AstContext::new(&relative_mod_symbols, &ast, &cfg);
        for node in &ast {
            node.transpile(&mut ast_ctx).map_err(|inner| {
                inner.with_source(&content, Some(path.to_string_lossy().to_string()))
            })?;
        }

        let rust_ast = ast_ctx.finish().map_err(|inner| {
            inner.with_source(&content, Some(path.to_string_lossy().to_string()))
        })?;

        codegen::fidelity_print(&rust_ast, &cfg).map_err(|inner| SerpentError::from(inner))
    });

    // Print the Rust AST as code
    Ok(out_str?)
}

fn parse_str_to_py_ast(src: &str) -> Result<Vec<python::NodeKind>, SerpentError> {
    // Parse Python source into a Python AST using RustPython
    info!("Parsing str into Python AST");
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
            compare_locations(a_loc, b_loc)
        })
        .collect::<Vec<python::NodeKind>>();

    Ok(py_nodes)
}

fn compare_locations(a_loc: &Location, b_loc: &Location) -> bool {
    if a_loc.row() < b_loc.row() {
        true
    } else if a_loc.row() == b_loc.row() {
        a_loc.column() < b_loc.column()
    } else {
        false
    }
}
