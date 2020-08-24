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
mod py_module;
mod transpile;

pub use crate::error::{SerpentError, TranspileError};
pub use crate::py_module::{import_module, ImportError, PyModule};
pub use crate::transpile::{transpile_python, TranspileOutput};

use ctor::ctor;
use std::path;

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
pub fn transpile_str(src: &str, infer_main: bool) -> Result<TranspileOutput> {
    transpile::transpile_python(src, infer_main)
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
pub fn transpile_module(dir_path: impl AsRef<path::Path>) -> Result<TranspileOutput> {
    let source_module = crate::import_module(dir_path);
    source_module?.transpile().map(|output| output)
}

pub fn transpile_file(file_path: impl AsRef<path::Path>) -> Result<TranspileOutput> {
    let path = file_path.as_ref();
    let source_file = crate::py_module::PyFile::from_path(path)?;
    source_file.transpile().map(|output| output)
}

// Enable color backtraces in binaries, tests and examples.
#[ctor]
fn init_color_backtrace() {
    color_backtrace::install();
}
