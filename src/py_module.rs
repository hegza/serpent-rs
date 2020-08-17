use std::{collections::HashMap, ffi, fs, io, path, result};

use crate::{transpile_v0::TranspileOutput, SerpentError, Transpile};
use log::{info, trace};
use thiserror::Error as ThisError;

/// A type alias for `Result<T, serpent::SerpentError>`.
pub type Result<T> = result::Result<T, SerpentError>;

/// Imports a possibly transpilable Python module from given directory.
///
/// Basic usage with `anyhow`.
///
/// ```
/// # const DIR: &str = "examples/py/black_scholes/";
/// use anyhow::{Context, Result};
/// use serpent::{transpile, Transpile, ProgramKind, PySource};
///
/// fn main() -> Result<()> {
///     let source_module = serpent::import_module(DIR);
///     let transpiled = source_module?
///         .transpile()
///         .context(format!("unable to transpile module"))?;
///
///     println!("Transpiled Rust source code:\n{}", &transpiled.out);
///     Ok(())
/// }
/// ```
pub fn import_module(dir_path: impl AsRef<path::Path>) -> Result<PyModule> {
    PyModule::from_dir_path(dir_path)
}

/// A buffer for messages concerning module import outcomes. Can be used to either inform the user
/// about what has been transpiled, or the transpiler about what to do in further processing steps.
/// This exists separately from logging and is meant to only report actionable items further down
/// the chain of transpilation.
type MessageBuffer = ();

/// Represents a Python module or a package loaded from a directory or a file.
#[derive(Debug)]
pub struct PyModule {
    /// A map of files by module path, for tracking Python imports and possible error backtrace.
    sources: HashMap<String, PyFile>,
    /// Module path of root in `sources`.
    has_root: bool,
    messages: MessageBuffer,
}

#[derive(Debug)]
pub struct PyFile {
    path: path::PathBuf,
    content: String,
}

impl PyModule {
    pub fn from_dir_path(dir_path: impl AsRef<path::Path>) -> Result<Self> {
        let path = dir_path.as_ref();

        // Find Python sources belonging to the module directory
        let py_files = collect_python_in_dir(path)?;

        // Determine the root path prefix
        // this fn is called with ./mod or ./mod/ or mod or /a/b/mod
        // init is: ./mod/__init__.py or mod/__init__.py or /a/b/mod/__init__.py
        // ergo prefix must be ./mod/ or mod/ or /a/b/mod/
        let root_path_prefix = dir_path.as_ref().to_owned();

        let mut has_root = false;

        // Check if a root module exists
        match py_files
            .iter()
            .find(|de| de.path().file_stem().expect("no filename") == "__init__")
        {
            Some(_de) => {
                info!("__init__ module found in source, module is likely runnable");
                has_root = true;
            }
            None => {
                info!("__init__ module *not* found in source, module is likely a library");
            }
        }

        // Construct the map of modules
        let sources = {
            let mut modules = HashMap::new();

            for f in &py_files {
                let path = &f.path();

                let relative_path_to_root = path
                    .strip_prefix(&root_path_prefix)
                    .expect("could not get relative path to root");
                let module_path = relative_path_to_root
                    .iter()
                    .map(|sub_path| {
                        sub_path
                            .to_str()
                            .expect("non-unicode filepath not expected here")
                    })
                    .collect::<Vec<&str>>()
                    .join(".");
                let content = fs::read_to_string(path)?;
                modules.insert(
                    module_path,
                    PyFile {
                        path: path.clone(),
                        content,
                    },
                );
            }

            modules
        };

        Ok(PyModule {
            sources,
            has_root,
            messages: (),
        })
    }
}

#[derive(ThisError, Debug)]
pub enum ImportError {
    #[error("IO error")]
    Io(#[from] io::Error),
}

impl Transpile for PyModule {
    fn transpile(&self) -> Result<TranspileOutput> {
        let mut rust_sources: Vec<String> = vec![];

        // Transpile file-by-file
        for file in self.sources.values() {
            let t_result = file.transpile()?;

            rust_sources.push(t_result.out);
        }

        let t_out = rust_sources.concat();
        let transpiled = TranspileOutput {
            out: t_out,
            messages: (),
        };
        Ok(transpiled)
    }
}

impl Transpile for PyFile {
    fn transpile(&self) -> Result<TranspileOutput> {
        // HACK: interpret everything as non-runnable for now
        // TODO: identify main() segment and interpret as a runnable file

        let transpiled = crate::transpile_v0::transpile_python(crate::PySource::Program(
            &self.content,
            crate::ProgramKind::NonRunnable,
        ))
        .map_err(|e| {
            crate::error::SerpentError::Transpile(self.path.to_string_lossy().to_string(), e)
        })?;

        let transpiled = TranspileOutput {
            out: transpiled,
            messages: (),
        };
        Ok(transpiled)
    }
}

/// Python file extensions for in-directory detection.
const PY_FILE_EXTS: [&str; 2] = ["py", "py3"];

fn collect_python_in_dir(dir: impl AsRef<path::Path>) -> Result<Vec<fs::DirEntry>> {
    let dir = dir.as_ref();
    let mut py_files = vec![];

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            py_files.append(&mut collect_python_in_dir(&path)?);
        } else {
            let name = entry.file_name().clone().into_string();
            if let Ok(name) = name {
                if is_python(&name) {
                    trace!(
                        "Adding {} as a source file in the module directory",
                        entry.path().to_str().unwrap()
                    );
                    py_files.push(entry);
                }
                // Not a Python file, note the ignored file in trace!
                else {
                    trace!("Ignored file in module directory as non-Python: {:?}", name);
                }
            } else {
                trace!(
                    "Ignored a non-unicode filepath in module directory: {:?}",
                    name
                );
            }
        }
    }
    Ok(py_files)
}

fn get_extension_from_filename(filename: &str) -> Option<&str> {
    path::Path::new(filename)
        .extension()
        .and_then(ffi::OsStr::to_str)
}

fn is_python(filename: impl AsRef<str>) -> bool {
    if let Some(ext) = get_extension_from_filename(filename.as_ref()) {
        PY_FILE_EXTS.contains(&ext)
    } else {
        false
    }
}
