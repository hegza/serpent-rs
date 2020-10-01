pub mod ast_to_ast;
mod codegen;
mod context;
mod parser_ext;
pub mod python;
pub mod rust;

use crate::config::{self, TranspileConfig};
use crate::{
    error::ApiError, output::ModPath, output::TranspiledFile, output::TranspiledModule,
    output::TranspiledString, PyModule,
};
use ast_to_ast::TranspileNode;
use context::{AstContext, ProgramContext, RustAst};
use itertools::Itertools;
use log::info;
use parser_ext::{parse_comments, parse_orphan_newlines};
use python::Node;
use rustc_ap_rustc_span::with_default_session_globals;
use rustpython_parser::{location::Location, parser as py_parser};
use std::{fs, path};

/// Transpiles a module from the given directory to Rust.
pub fn transpile_module_dir(
    dir_path: impl AsRef<path::Path>,
) -> Result<TranspiledModule, ApiError> {
    let py_module = PyModule::from_dir_path(dir_path)?;
    let files = py_module.files();

    // Transpile files one-by-one
    let mut prog_ctx = ProgramContext::new(&py_module);
    let mut transpiled_sources = Vec::with_capacity(files.len());
    for py_path in &files {
        // Transpile this file
        let transpiled = transpile_file(py_path, &mut prog_ctx)?;
        let mod_path =
            ModPath::from_py_module_symbol(py_module.module_symbol_for_file(py_path).unwrap());

        transpiled_sources.push((mod_path, transpiled));

        // Next file
        prog_ctx.advance();
    }

    let out = TranspiledModule::from_paths_and_files(transpiled_sources);

    Ok(out)
}

type PythonAst = Vec<python::NodeKind>;

fn ast_to_ast(
    py_ast: &PythonAst,
    relative_mod_symbols: &[String],
    cfg: &TranspileConfig,
) -> Result<RustAst, crate::error::TranspileNodeError> {
    let mut ast_ctx = AstContext::new(&relative_mod_symbols, py_ast, cfg);
    for node in py_ast {
        node.transpile(&mut ast_ctx)?;
    }

    ast_ctx.finish()
}

fn codegen(rust_ast: &RustAst, cfg: &TranspileConfig) -> Result<String, ApiError> {
    codegen::ast_to_rust(rust_ast, cfg).map_err(|inner| ApiError::from(inner))
}

/// Transpiles given Python source code to Rust source code assuming no other
/// module context.
///
/// # Arguments
pub fn transpile_str(src: &str, cfg: &TranspileConfig) -> Result<TranspiledString, ApiError> {
    let ast = parse_str_to_py_ast(&src)?;

    // The Python program is a sequence of statements, comments, and newlines
    // which can be translated to Rust
    let dummy_relat_modules = vec![];
    let rust_ast = ast_to_ast(&ast, &dummy_relat_modules, &cfg)
        .map_err(|inner| inner.with_source(&src, None))?;

    // Print the Rust AST as code
    let out_str = codegen(&rust_ast, &cfg)?;

    let out = TranspiledString {
        python_source: src.to_owned(),
        python_ast: ast,
        rust_ast,
        rust_target: out_str,
    };
    Ok(out)
}

fn transpile_file(
    path: &path::PathBuf,
    ctx: &mut ProgramContext,
) -> Result<TranspiledFile, ApiError> {
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
    let result: Result<(Vec<rust::NodeKind>, String), ApiError> =
        with_default_session_globals(|| {
            let rust_ast = ast_to_ast(&ast, &relative_mod_symbols, &cfg).map_err(|inner| {
                inner.with_source(&content, Some(path.to_string_lossy().to_string()))
            })?;

            // Print the Rust AST as code
            let rust_code = codegen(&rust_ast, &cfg)?;
            Ok((rust_ast, rust_code))
        });

    let (rust_ast, out_str) = result?;

    let out = TranspiledFile(
        path.to_owned(),
        TranspiledString {
            python_source: content,
            python_ast: ast,
            rust_ast,
            rust_target: out_str,
        },
    );

    Ok(out)
}

fn parse_str_to_py_ast(src: &str) -> Result<Vec<python::NodeKind>, ApiError> {
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

    // Parse orphan newlines with their locations
    let newlines = parse_orphan_newlines(src);
    let newline_nodes = newlines
        .into_iter()
        .map(|row| python::NodeKind::Newline(Location::new(row, 1)));

    // Re-arrange comments and statements
    let py_nodes = stmt_nodes
        .merge_by(comment_nodes, |a, b| {
            let (a_loc, b_loc) = (a.location(), b.location());
            compare_locations(a_loc, b_loc)
        })
        .merge_by(newline_nodes, |a, b| {
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
