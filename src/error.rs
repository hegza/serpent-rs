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

use std::{fmt, io};

use rustpython_parser::{error::ParseError, location::Location};
use thiserror::Error as ThisError;

/// An error that occurred due to a top-level API call.
#[derive(ThisError, Debug)]
pub enum SerpentError {
    /// An I/O error that occurred while reading a Python source file. All IO
    /// errors are from Python files, as long as we only parse Python files.
    /// This may change one day.
    #[error("IO error while reading Python source")]
    Io(#[from] io::Error),
    /// A parsing error that occurred while parsing the string contents of a
    /// file into a Python AST with RustPython.
    #[error("Python parse error")]
    Parse(#[from] ParseError),
    /// An error that occurred while transpiling a Python file into a Rust AST.
    #[error("Transpile error")]
    Transpile(#[from] TranspileError),
    /// An error that occurred while expanding a Rust AST into Rust source code.
    #[error("Rust AST expansion error")]
    Expand(#[from] ExpandError),
    #[error(
        "Requested for line {requested} in {file:?} but the file has {actual_line_count} lines"
    )]
    LineParameter {
        requested: usize,
        file: String,
        actual_line_count: usize,
    },
    /// Hints that destructuring should not be exhaustive.
    ///
    /// This enum may grow additional variants, so this makes sure clients
    /// don't count on exhaustive matching. (Otherwise, adding a new variant
    /// could break existing code.)
    #[doc(hidden)]
    #[error("Unreachable")]
    __Nonexhaustive,
}

#[derive(ThisError, Debug, Clone)]
pub struct TranspileError {
    pub location: Location,
    pub filename: Option<String>,
    pub line: String,
    pub source: TranspileNodeError,
}

impl SerpentError {
    /// Return the location for this error, if one exists.
    ///
    /// This is a convenience function that permits callers to easily access
    /// the location on an error without doing case analysis on `SerpentError`.
    pub fn location(&self) -> Option<&Location> {
        match *self {
            SerpentError::Parse(ref err) => Some(&err.location),
            SerpentError::Transpile(ref err) => Some(&err.location),
            _ => None,
        }
    }

    /// Returns true if this is an I/O error.
    ///
    /// If this is true, the underlying `SerpentError` is guaranteed to be
    /// `SerpentError::Io`.
    pub fn is_io_error(&self) -> bool {
        match *self {
            SerpentError::Io(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for TranspileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(file) = &self.filename {
            write!(
                f,
                "Could not transpile line {} column {}, in {}:{}",
                self.location.row(),
                self.location.column(),
                file,
                self.location.visualize(&self.line, "")
            )
        } else {
            write!(
                f,
                "Could not transpile line {} column {}:\n\t`{}`",
                self.location.row(),
                self.location.column(),
                self.line
            )
        }
    }
}

/// An error that occurred while transpiling a Python AST node into Rust.
#[derive(ThisError, Debug, Clone)]
pub enum TranspileNodeError {
    /// Transform for this Python AST node was not implemented.
    Unimplemented {
        debug: String,
        location: Option<Location>,
    },
    UnresolvedBlock(usize),
}

impl TranspileNodeError {
    pub fn unimplemented<D: fmt::Debug>(
        node: &D,
        location: Option<Location>,
    ) -> TranspileNodeError {
        TranspileNodeError::Unimplemented {
            debug: format!("{:?}", node),
            location,
        }
    }

    pub fn location(&self) -> Option<&Location> {
        match self {
            TranspileNodeError::Unimplemented { location, .. } => location.as_ref(),
            TranspileNodeError::UnresolvedBlock(_) => None,
        }
    }

    pub fn with_source(self, src: &str, filename: Option<String>) -> TranspileError {
        let location = self.location().unwrap().clone();
        let line = src
            .lines()
            .nth(location.row() - 1)
            .expect(&format!("source has less than {} rows", location.row()))
            .to_owned();
        TranspileError {
            filename,
            location,
            line,
            source: self,
        }
    }
}

impl fmt::Display for TranspileNodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TranspileNodeError::Unimplemented { debug, location } => match location {
                Some(loc) => write!(f, "Unimplemented node: {:?} at {}", debug, loc),
                None => write!(f, "Unimplemented node: {:?}", debug),
            },
            TranspileNodeError::UnresolvedBlock(depth) => write!(
                f,
                "Incomplete block recursion, depth is non-zero ({}) at .finish()",
                depth
            ),
        }
    }
}

#[derive(ThisError, Debug, Clone)]
pub enum ExpandError {
    /// Fidelity print is not implemented for this item
    #[error("no implementation for node: {0}")]
    Unimplemented(String),
}
