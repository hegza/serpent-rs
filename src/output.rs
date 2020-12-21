pub use crate::py_module::PyModule;
use crate::{
    fmt::OpaqueDebug,
    fmt::OpaqueFmt,
    transpile::{
        python::{self, Node},
        rust,
    },
};
use python::PythonAst;
use rustc_ap_rustc_span::with_default_session_globals;

use std::{fmt, ops::Deref, path};

/// A module of transpiled Rust with associated AST's in Python and Rust.
/// Comprises `TranspiledFile`'s.
#[derive(Debug, Default)]
pub struct TranspiledModule {
    /// Module paths as a vector of strings.
    pub paths: Vec<ModPath>,
    pub files: Vec<TranspiledFile>,
}

impl TranspiledModule {
    pub fn from_paths_and_files(
        paths_and_files: Vec<(ModPath, TranspiledFile)>,
    ) -> TranspiledModule {
        let (paths, files) = paths_and_files.into_iter().unzip();
        TranspiledModule { paths, files }
    }

    pub fn files(&self) -> &[TranspiledFile] {
        &self.files
    }
    pub fn files_mut(&mut self) -> &mut Vec<TranspiledFile> {
        &mut self.files
    }

    pub fn file_by_mod_path(&self, mod_path: &ModPath) -> Option<&TranspiledFile> {
        for (idx, mod_path2) in self.paths.iter().enumerate() {
            if mod_path == mod_path2 {
                return Some(&self.files[idx]);
            }
        }
        None
    }
    pub fn file_by_file_path(&self, path: &path::Path) -> Option<&TranspiledFile> {
        for file in self.files.iter() {
            if file.source_path() == path {
                return Some(&file);
            }
        }
        None
    }
}

/// Contains the original source path and the transpiled output.
#[derive(Debug)]
pub struct TranspiledFile {
    pub source_path: path::PathBuf,
    pub kind: TranspiledFileKind,
    pub content: TranspiledString,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TranspiledFileKind {
    Normal,
    /// This file was identified as lib.rs during transpilation
    LibRs,
    /// This file was identified as main.rs during transpilation
    MainRs,
}

impl Default for TranspiledFileKind {
    fn default() -> Self {
        TranspiledFileKind::Normal
    }
}

#[derive(Debug)]
pub struct TranspiledString {
    pub python_source: String,
    pub python_ast: PythonAst,
    pub rust_ast: Vec<rust::NodeKind>,
    pub rust_target: String,
}

impl TranspiledFile {
    /// Attempts to find the top level node span for the given line.
    fn find_top_level_span_for_line(
        &self,
        line_no: usize,
    ) -> Result<(usize, usize), crate::ApiError> {
        let py_lines = self.content().python_source.lines();
        let line_count = py_lines.count();

        // Check that the file has at least this many lines
        if line_no > line_count {
            return Err(crate::error::TraceError::LineParameter {
                requested: line_no,
                file: self.source_path().to_str().unwrap().to_owned(),
                actual_line_count: line_count,
            }
            .into());
        };

        // Find the top-level AST node span that contains this line
        let (mut start_line, mut end_line) = (0, 0);

        let nodes = self
            .content()
            .python_ast
            .iter()
            .filter(|&node| match node {
                python::NodeKind::Statement(_) => true,
                _ => false,
            })
            .collect::<Vec<&python::NodeKind>>();
        for windows in nodes.windows(2) {
            let cur_node = &windows[0];
            let next_node = &windows[1];

            let row_of_cur = cur_node.location().row();
            let row_of_next = next_node.location().row();

            if line_no >= row_of_cur && line_no < row_of_next {
                start_line = row_of_cur;
                end_line = row_of_next - 1;
            }
        }
        // Check last one separately
        if let Some(last) = nodes.last() {
            let row_of_last = last.location().row();
            if line_no >= row_of_last {
                start_line = row_of_last;
                end_line = line_count + 1;
            }
        }

        if start_line == 0 || end_line == 0 {
            return Err(crate::error::TraceError::SpanNotFound(line_no).into());
        }

        Ok((start_line, end_line))
    }

    /// HACK: this is a workaround that helps me debug things
    pub fn trace_steps_for_line(
        &self,
        line: usize,
        top_only: bool,
    ) -> Result<Vec<String>, crate::ApiError> {
        let (start_line, end_line) = self.find_top_level_span_for_line(line)?;

        let py_lines = self.content().python_source.lines();

        let v: Result<Vec<String>, crate::ApiError> = with_default_session_globals(|| {
            // FIXME: currently using single-line transpile to get ahead quickly
            // Call to unwrap is safe because we have verified the length of py_lines.
            let lines = py_lines.map(str::to_owned).collect::<Vec<String>>();
            let start_line_idx = start_line
                .checked_sub(1)
                .expect(&format!("cannot subtract one from {}", start_line));
            let end_line_idx = end_line
                .checked_sub(1)
                .expect(&format!("cannot subtract one from {}", end_line));
            let line_str = lines[start_line_idx..end_line_idx].join("\n");
            let transpiled = crate::transpile_str(&line_str)?;

            /*
            let py_src = {
                let mut line_thresholds = self
                    // TODO: I should visit locations within the tree
                    .python_ast
                    .iter()
                    .map(|node| node.location().row())
                    .collect::<Vec<usize>>();

                line_thresholds.sort();

                for threshold in line_thresholds {}
            };
            */

            // We can't leak this type because it depends on transpiler session globals. We
            // format it and return as string instead.
            let steps = TranspileStmtSteps {
                py_src: line_str.to_owned(),
                py_ast: transpiled.python_ast,
                rs_ast: transpiled.rust_ast,
                rs_tgt: transpiled.rust_target,
            };
            let (py_ast, rs_ast) = {
                if top_only {
                    (
                        format!("{:?}", OpaqueFmt(|f| steps.py_ast.opaque_fmt(f))),
                        format!("{:?}", OpaqueFmt(|f| steps.rs_ast.opaque_fmt(f))),
                    )
                } else {
                    (format!("{:?}", steps.py_ast), format!("{:?}", steps.rs_ast))
                }
            };

            Ok(vec![steps.py_src, py_ast, rs_ast, steps.rs_tgt])
        });

        Ok(v?)
    }

    /// HACK: this is a workaround that helps me debug things.
    pub fn trace_top_for_file(&self) -> Result<Vec<String>, crate::ApiError> {
        let v: Result<Vec<String>, crate::ApiError> = with_default_session_globals(|| {
            // FIXME: currently using single-line transpile to get ahead quickly
            // Call to unwrap is safe because we have verified the length of py_lines.
            let transpiled = crate::transpile_str(&self.python_source)?;

            /*
            let py_src = {
                let mut line_thresholds = self
                    // TODO: I should visit locations within the tree
                    .python_ast
                    .iter()
                    .map(|node| node.location().row())
                    .collect::<Vec<usize>>();

                line_thresholds.sort();

                for threshold in line_thresholds {}
            };
            */

            // We can't leak this type because it depends on transpiler session globals. We
            // format it and return as string instead.
            let (py_ast, rs_ast) = {
                (
                    format!("{:?}", OpaqueFmt(|f| transpiled.python_ast.opaque_fmt(f))),
                    format!("{:?}", OpaqueFmt(|f| transpiled.rust_ast.opaque_fmt(f))),
                )
            };

            Ok(vec![
                transpiled.python_source,
                py_ast,
                rs_ast,
                transpiled.rust_target,
            ])
        });

        Ok(v?)
    }

    /// HACK: this is a workaround that helps me debug things
    pub fn trace_top(&self, line: Option<usize>) -> Result<Vec<String>, crate::ApiError> {
        match line {
            Some(line) => self.trace_steps_for_line(line, true),
            None => self.trace_top_for_file(),
        }
    }

    pub fn source_path(&self) -> &path::Path {
        &self.source_path
    }
    pub fn content(&self) -> &TranspiledString {
        &self.content
    }
}

impl TranspiledString {
    pub fn as_str(&self) -> &str {
        &self.rust_target
    }
    /// Converts self into a String without copying or allocating
    pub fn into_string(self) -> String {
        self.rust_target
    }
}

/// Represents a language independent module path with respect to crate root,
/// eg.
/// - ["main"]
/// - ["__init__"] // might be converted to "main" depending on options
/// - ["algorithm", "black_scholes"]
/// - ["algorithm", "black_scholes", "test"]
#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct ModPath(Vec<String>);

impl ModPath {
    pub fn from_py_module_symbol(symbol: &str) -> ModPath {
        ModPath(
            symbol
                .split(".")
                .map(str::to_owned)
                .collect::<Vec<String>>(),
        )
    }
    /// Constructs a module path from a crate root relative path. Eg.
    /// `"file.py"` -> `["file"]` or `"lib/file.py"` -> `["lib", "file"]`.
    pub fn from_relative_path(_rel_path: &path::Path) -> ModPath {
        unimplemented!()
    }
}

impl fmt::Display for ModPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl Deref for TranspiledFile {
    type Target = TranspiledString;

    fn deref(&self) -> &Self::Target {
        &self.content()
    }
}

#[derive(Debug)]
struct TranspileStmtSteps {
    py_src: String,
    py_ast: PythonAst,
    rs_ast: Vec<rust::NodeKind>,
    rs_tgt: String,
}
