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
//! use serpent::{transpile, Transpile, ProgramKind, PySource};
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
mod transpile_v0;
mod transpile_v1;

pub use crate::error::{SerpentError, TranspileError};
pub use crate::py_module::{import_module, ImportError, PyModule};
pub use crate::transpile_v0::Transpile;

use ctor::ctor;
use std::result;

/// A type alias for `Result<T, serpent::SerpentError>`.
pub type Result<T> = result::Result<T, SerpentError>;

/// A string representing a piece of Python source code.
pub enum PySource<'a> {
    Program(&'a str, ProgramKind),
}

/// Represents if the Python program should be interpreted as runnable or as
/// non-runnable (like a library).
pub enum ProgramKind {
    /// Interpret the program as something runnable. Causes an "fn main()" entry
    /// point to be generated in Rust source code.
    Runnable,
    /// Interpret the program as not-runnable. Causes freestanding statements to
    /// be interpreted as const declarations.
    NonRunnable,
}

// TODO: move as part of Transpile
/// Transpiles given Python source to Rust.
///
/// # Examples
///
/// Basic usage:
///
/// ```no_run
/// # const DIR: &str = "examples/py/black_scholes/";
/// use std::fs;
/// use serpent::{transpile, PySource, ProgramKind};
///
/// # fn foo() -> serpent::Result<String> {
///     let source = fs::read_to_string("__init__.py")?;
///
///     let result = transpile(PySource::Program(&source, ProgramKind::Runnable))?;
///     println!("Result:\n{}", &result);
/// # Ok(result)
/// # }
/// ```
pub fn transpile_v0(src: PySource) -> Result<String> {
    transpile_v0::transpile_python(src).map_err(|e| SerpentError::Transpile("input".to_owned(), e))
}

// Enable color backtraces in binaries, tests and examples.
#[ctor]
fn init_color_backtrace() {
    color_backtrace::install();
}
