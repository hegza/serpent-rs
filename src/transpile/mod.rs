pub mod ast_to_ast;
mod codegen;
mod context;
mod parser_ext;
pub mod python;
mod rs_ast;
pub mod rust;

use crate::{
    config::{self, TranspileConfig},
    output::TranspiledFileKind,
};
use crate::{
    error::ApiError, output::ModPath, output::TranspiledFile, output::TranspiledModule,
    output::TranspiledString, PyModule,
};
use ast_to_ast::TranspileNode;
use config::InferOption;
use context::{AstContext, ProgramContext, RustAst};
use fs_err as fs;
use log::{debug, info, warn};
use python::PythonAst;
use rustc_ap_rustc_span::with_default_session_globals;
use std::{ffi, path};

/// Transpiles a module from the given directory to Rust.
pub fn transpile_module_dir(
    dir_path: impl AsRef<path::Path>,
    cfg: &TranspileConfig,
) -> Result<TranspiledModule, ApiError> {
    let dir_path = dir_path.as_ref();
    info!(
        "Transpiling module from directory {:?} with config {:?}",
        dir_path, cfg
    );

    let py_module = PyModule::from_dir_path(dir_path)?;
    let mut files = py_module.files();

    // Stabilize file-order using std::cmp::Eq for std::path::PathBuf
    files.sort();

    // Transpile files one-by-one
    let mut prog_ctx = ProgramContext::new(&py_module);
    let mut transpiled_sources = Vec::with_capacity(files.len());
    for py_path in &files {
        // Transpile this file
        let transpiled = transpile_file(py_path, &mut prog_ctx, cfg)?;
        let mod_path =
            ModPath::from_py_module_symbol(py_module.module_symbol_for_file(py_path).unwrap());

        transpiled_sources.push((mod_path, transpiled));

        // Next file
        prog_ctx.advance();
    }

    let out = TranspiledModule::from_paths_and_files(transpiled_sources);

    Ok(out)
}

fn ast_to_ast(
    py_ast: &PythonAst,
    relative_mod_symbols: &[String],
    cfg: &TranspileConfig,
) -> Result<RustAst, crate::error::TranspileNodeError> {
    let mut ast_ctx = AstContext::new(&relative_mod_symbols, &py_ast.0, cfg);
    for node in py_ast.0.iter() {
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
    let ast = src.parse::<PythonAst>()?;

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

/// Transpiles a file from given path. Returns the transpiled file or ApiError.
/// This is private because files cannot be transpiled without module context.
/// Call transpile_str for file contents instead.
fn transpile_file(
    path: &path::PathBuf,
    ctx: &mut ProgramContext,
    cfg: &TranspileConfig,
) -> Result<TranspiledFile, ApiError> {
    info!("Parsing {:?} into Python AST", path);

    let kind = maybe_check_file_kind(path, cfg);

    // Read file and normalize line endings (into '\n')
    let content =
        normalize_line_endings::normalized(fs::read_to_string(path)?.chars()).collect::<String>();

    // Parse file into an AST
    let ast = content.parse::<PythonAst>()?;

    // Resolve relative mod symbols so that AST context can figure out which modules
    // are local and which are foreign
    let relative_mod_symbols = ctx.source_module().resolve_mod_symbols_relative_to(path);

    // Transform the Python AST into a Rust AST
    let result: Result<(Vec<rust::NodeKind>, String), ApiError> =
        with_default_session_globals(|| {
            let mut rust_ast = ast_to_ast(&ast, &relative_mod_symbols, &cfg).map_err(|inner| {
                inner.with_source(&content, Some(path.to_string_lossy().to_string()))
            })?;

            prune_top_level_statements(&mut rust_ast);

            // Insert `mod {}` to crate root.
            maybe_insert_crate_local_mods(
                &kind,
                &mut rust_ast,
                &ctx.source_module().resolve_global_mod_symbols(),
            );

            // Print the Rust AST as code
            let rust_code = codegen(&rust_ast, &cfg)?;
            Ok((rust_ast, rust_code))
        });

    let (rust_ast, out_str) = result?;

    let out = TranspiledFile {
        source_path: path.to_owned(),
        kind,
        content: TranspiledString {
            python_source: content,
            python_ast: ast,
            rust_ast,
            rust_target: out_str,
        },
    };

    Ok(out)
}

/// Checks for special file kinds based on configuration options.
fn maybe_check_file_kind(path: &path::Path, cfg: &TranspileConfig) -> TranspiledFileKind {
    if path.file_stem() == Some(ffi::OsStr::new("__init__")) {
        if cfg.infer_options.contains(&InferOption::InitFileIntoMainRs) {
            info!("Detected {:?} as source for main.rs", path);
            TranspiledFileKind::MainRs
        } else if cfg.infer_options.contains(&InferOption::InitFileIntoLibRs) {
            info!("Detected {:?} as source for lib.rs", path);
            TranspiledFileKind::LibRs
        } else {
            TranspiledFileKind::Normal
        }
    } else {
        TranspiledFileKind::Normal
    }
}

fn maybe_insert_crate_local_mods(
    kind: &TranspiledFileKind,
    rust_ast: &mut RustAst,
    mod_symbols: &[String],
) {
    // TODO: do not use kind, but instead determine the "crate root" using a
    // higher-level structure, then insert the mods to the crate root.
    match kind {
        TranspiledFileKind::LibRs | TranspiledFileKind::MainRs => {
            rs_ast::insert_crate_local_mods(rust_ast, mod_symbols);
        }
        _ => {}
    }
}

/// Prunes top-level statements because they are forbidden in Rust.
// HACK: needs more detail, because this would also prune top-level items and
// Python comments like `""" comment """`.
fn prune_top_level_statements(rust_ast: &mut RustAst) {
    rust_ast.retain(|node| match node {
        rust::NodeKind::Stmt(stmt) => {
            warn!("Removed a top-level statement. Rust does not support top-level statements.",);
            debug!("Removed top-level statement: {:?}", stmt);
            false
        }
        _ => true,
    });
}
