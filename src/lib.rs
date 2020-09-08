//! Transpiles Python source code into Rust source code. Opinionated transforms
//! are kept to a minimum.
//!
//! # Examples
//!
//! Basic usage with `anyhow`.
//!
//! ```
//! # const DIR: &str = "examples/py/black_scholes/";
//! use anyhow::{Context, Result};
//! use serpent::import_module;
//!
//! fn main() -> Result<()> {
//!     let source_module = serpent::import_module(DIR);
//!     let transpiled = source_module?
//!         .transpile()
//!         .context(format!("unable to transpile module"))?;
//!
//!     println!("Transpiled Rust source code:\n{}", &transpiled.out);
//!     Ok(())
//! }
//! ```
mod error;
mod output;
mod py_module;
mod transpile;

pub use crate::error::{SerpentError, TranspileError};
pub use crate::py_module::{ImportError, PyModule};

use ctor::ctor;
use output::TranspiledString;
use std::{fs, path};
use transpile::transpile_module_dir;

/// A type alias for `Result<T, serpent::SerpentError>`. All API functions
/// return a SerpentError.
pub type Result<T> = std::result::Result<T, SerpentError>;

/// Enum for different kinds of Python programs: runnables and libraries.
#[derive(PartialEq, Clone)]
pub enum ProgramKind {
    /// Interpret the program as something runnable. Causes an "fn main()" entry
    /// point to be generated in Rust source code.
    Runnable,
    /// Interpret the program as not-runnable. Causes freestanding statements to
    /// be interpreted as const declarations.
    Library,
}

/// Transpiles given Python source to Rust.
///
/// # Examples
///
/// Basic usage:
///
/// ```no_run
/// # const DIR: &str = "examples/py/black_scholes/";
/// use std::fs;
/// use serpent::transpile_str;
///
/// # fn foo() -> serpent::Result<String> {
///     let source = fs::read_to_string("__init__.py")?;
///
///     let result = transpile_str(&source, true)?;
///     println!("Result:\n{}", &result);
/// #    Ok(result)
/// # }
/// ```
pub fn transpile_str(src: String, infer_main: bool) -> Result<TranspiledString> {
    transpile::transpile_str(src, infer_main)
}

/// Transpiles a Python module from given directory to Rust.
/// TODO: produce a project instead of a string.
///
/// Basic usage with `anyhow`.
///
/// ```
/// # const DIR: &str = "examples/py/black_scholes/";
/// use anyhow::{Context, Result};
///
/// fn main() -> Result<()> {
///     let transpiled = serpent::transpile_module(DIR)
///         .context(format!("unable to transpile module"))?;
///
///     println!("Transpiled Rust source code:\n{}", &transpiled);
/// #    Ok(())
/// }
/// ```
pub fn transpile_module(dir_path: impl AsRef<path::Path>) -> Result<Vec<(path::PathBuf, String)>> {
    transpile_module_dir(dir_path)
}

pub fn transpile_file(file_path: impl AsRef<path::Path>) -> Result<TranspiledString> {
    let path = file_path.as_ref();
    let content = fs::read_to_string(path)?;
    transpile_str(content, false)
}

/// Transpiles a single line in a file without any additional context
pub fn transpile_standalone_line_in_file(
    file_path: impl AsRef<path::Path>,
    line: u64,
) -> Result<TranspiledString> {
    let path = file_path.as_ref();
    let content = fs::read_to_string(path)?;
    match content.lines().nth(line as usize) {
        Some(line_content) => transpile_str(line_content.to_owned(), false),
        None => Err(SerpentError::LineParameter {
            requested: line,
            file: path.to_str().unwrap().to_owned(),
            actual_line_count: content.lines().count() as u64,
        }),
    }
}

// Enable color backtraces in binaries, tests and examples.
#[ctor]
fn init_color_backtrace() {
    color_backtrace::install();
}
