//! Transpiles Python source code into Rust source code. Opinionated transforms are kept to a minimum.
mod error;
mod transpile;

pub use crate::error::{Error, ErrorKind, Result};

/// A string representing a piece of Python source code.
pub enum PySource<'a> {
    Program(&'a str, ProgramKind),
}

/// Represents if the Python program should be interpreted as runnable or as non-runnable (like
/// a library).
pub enum ProgramKind {
    /// Interpret the program as something runnable. Causes an "fn main()" entry point to be
    /// generated in Rust source code.
    Runnable,
    /// Interpret the program as not-runnable. Causes freestanding statements to be interpreted as
    /// const declarations.
    NonRunnable,
}

/// Transpiles given Python source to Rust.
///
/// # Examples
///
/// Basic usage:
///
/// ```no_run
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
pub fn transpile(src: PySource) -> Result<String> {
    transpile::transpile_python(src)
}
