//! Error boilerplate and documentation adapted from the great [implementation](https://github.com/BurntSushi/rust-csv/blob/master/src/error.rs) by Andrew Gallant
//! (BurntSushi). Visited 30.4.2020. License of original source is reproduced
//! below, though separate from the licensing of the `serpent` library.
//!
//! The MIT License (MIT)
//!
//! Copyright (c) 2015 Andrew Gallant
//!
//! Permission is hereby granted, free of charge, to any person obtaining a copy
//! of this software and associated documentation files (the "Software"), to
//! deal in the Software without restriction, including without limitation the
//! rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//! sell copies of the Software, and to permit persons to whom the Software is
//! furnished to do so, subject to the following conditions:
//!
//! The above copyright notice and this permission notice shall be included in
//! all copies or substantial portions of the Software.
//!
//! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//! IN THE SOFTWARE.

use std::{fmt, io, result};

use crate::transpile::{identify_lines::IdentifyLinesError, recontextualize::RecontextualizeError};
use rustpython_parser::{error::ParseError, location::Location};
use syn::Error as SynError;
use thiserror::Error as ThisError;

/// A type alias for `Result<T, serpent::TranspileError>`.
pub type Result<T> = result::Result<T, TranspileError>;

/// The specific type of an error.
#[derive(ThisError, Debug)]
pub enum TranspileError {
    /// An I/O error that occurred while reading Python source file. All IO errors are from Python files, as long as we only parse Python files. This may change one day.
    #[error("IO error while reading Python source")]
    Io(#[from] io::Error),
    /// A parsing error that occurred while parsing a string into a Python AST
    /// with RustPython.
    #[error("Python parse error")]
    Parse(#[from] ParseError),
    /// A parsing error that occurred while parsing a string into a Rust AST
    /// with syn.
    #[error("Rust parse error")]
    ParseRust(#[from] SynError),
    /// A parsing error that occurred while identifying line kinds from a Python
    /// source.
    #[error("Identify lines error: {0}")]
    IdentifyLines(IdentifyLinesError),
    /// An error that occurred while reconstructing context for parsed Python
    /// source.
    #[error("Recontextualize error: {0}")]
    Recontextualize(RecontextualizeError),
    /// An error that occurred while transpiling the Python AST into Rust. This
    /// line could not be transpiled.
    #[error("Transpile error on line {line_no}: {reason}\n\t`{line}`")]
    Transpile {
        line: String,
        line_no: usize,
        reason: TranspileNodeError,
    },
    /// Hints that destructuring should not be exhaustive.
    ///
    /// This enum may grow additional variants, so this makes sure clients
    /// don't count on exhaustive matching. (Otherwise, adding a new variant
    /// could break existing code.)
    #[doc(hidden)]
    #[error("unreachable")]
    __Nonexhaustive,
}

impl TranspileError {
    /// Return the location for this error, if one exists.
    ///
    /// This is a convenience function that permits callers to easily access
    /// the location on an error without doing case analysis on `TranspileError`.
    pub fn location(&self) -> Option<&Location> {
        match *self {
            TranspileError::Parse(ref err) => Some(&err.location),
            _ => None,
        }
    }

    /// Returns true if this is an I/O error.
    ///
    /// If this is true, the underlying `TranspileError` is guaranteed to be
    /// `TranspileError::Io`.
    pub fn is_io_error(&self) -> bool {
        match *self {
            TranspileError::Io(_) => true,
            _ => false,
        }
    }
}

#[derive(ThisError, Debug)]
pub enum TranspileNodeError {
    /// An error that occurred while transpiling the Python AST into Rust. A
    /// transform for this Python AST node was not implemented.
    Unimplemented {
        node: String,
        location: Option<Location>,
    },
}

impl TranspileNodeError {
    pub fn unimplemented<D: fmt::Debug>(
        node: &D,
        location: Option<Location>,
    ) -> TranspileNodeError {
        TranspileNodeError::Unimplemented {
            node: format!("{:?}", node),
            location,
        }
    }
}

impl fmt::Display for TranspileNodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TranspileNodeError::Unimplemented { node, location } => match location {
                // TODO: format location with {} / fmt::Display
                Some(loc) => write!(f, "Unimplemented node: {:?} at {:?}", node, loc),
                None => write!(f, "Unimplemented node: {:?}", node),
            },
        }
    }
}
