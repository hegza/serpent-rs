//! Error boilerplate and documentation adapted from the great [implementation](https://github.com/BurntSushi/rust-csv/blob/master/src/error.rs) by Andrew Gallant
//! (BurntSushi). Visited 30.4.2020. License of original source is reproduced below, though separate
//! from the licensing of the `serpent` library.
//!
//! The MIT License (MIT)
//!
//! Copyright (c) 2015 Andrew Gallant
//!
//! Permission is hereby granted, free of charge, to any person obtaining a copy
//! of this software and associated documentation files (the "Software"), to deal
//! in the Software without restriction, including without limitation the rights
//! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//! copies of the Software, and to permit persons to whom the Software is
//! furnished to do so, subject to the following conditions:
//!
//! The above copyright notice and this permission notice shall be included in
//! all copies or substantial portions of the Software.
//!
//! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//! THE SOFTWARE.

use std::error::Error as StdError;
use std::fmt;
use std::io;
use std::result;

use crate::transpile::{identify_lines::IdentifyLinesError, recontextualize::RecontextualizeError};
use rustpython_parser::error::ParseError;
use rustpython_parser::location::Location;

/// A type alias for `Result<T, serpent::Error>`.
pub type Result<T> = result::Result<T, Error>;

/// An error that can occur when transpiling Python to Rust.
#[derive(Debug)]
pub struct Error(Box<ErrorKind>);

impl Error {
    /// A crate private constructor for `Error`.
    pub(crate) fn new(kind: ErrorKind) -> Error {
        Error(Box::new(kind))
    }

    /// Return the specific type of this error.
    pub fn kind(&self) -> &ErrorKind {
        &self.0
    }

    /// Unwrap this error into its underlying type.
    pub fn into_kind(self) -> ErrorKind {
        *self.0
    }

    /// Returns true if this is an I/O error.
    ///
    /// If this is true, the underlying `ErrorKind` is guaranteed to be
    /// `ErrorKind::Io`.
    pub fn is_io_error(&self) -> bool {
        match *self.0 {
            ErrorKind::Io(_) => true,
            _ => false,
        }
    }

    /// Return the location for this error, if one exists.
    ///
    /// This is a convenience function that permits callers to easily access
    /// the location on an error without doing case analysis on `ErrorKind`.
    pub fn location(&self) -> Option<&Location> {
        self.0.location()
    }
}

/// The specific type of an error.
#[derive(Debug)]
pub enum ErrorKind {
    /// An I/O error that occurred while reading Python source file.
    Io(io::Error),
    /// A parsing error that occurred while parsing a string into a Python AST
    /// with RustPython.
    Parse(ParseError),
    /// A parsing error that occurred while identifying line kinds from a Python
    /// source.
    IdentifyLines(IdentifyLinesError),
    /// An error that occurred while reconstructing context for parsed Python source.
    Recontextualize(RecontextualizeError),
    /// An error that occurred while transpiling the Python AST into Rust. This line could not be
    /// transpiled.
    Transpile {
        line: String,
        line_no: usize,
        reason: TranspileError,
    },
    /// Hints that destructuring should not be exhaustive.
    ///
    /// This enum may grow additional variants, so this makes sure clients
    /// don't count on exhaustive matching. (Otherwise, adding a new variant
    /// could break existing code.)
    #[doc(hidden)]
    __Nonexhaustive,
}

impl ErrorKind {
    /// Return the location for this error, if one exists.
    ///
    /// This is a convenience function that permits callers to easily access
    /// the location on an error without doing case analysis on `ErrorKind`.
    pub fn location(&self) -> Option<&Location> {
        match *self {
            ErrorKind::Parse(ref err) => Some(&err.location),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::new(ErrorKind::Io(err))
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Error {
        Error::new(ErrorKind::Parse(err))
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match *self.0 {
            ErrorKind::Parse(ref err) => Some(err),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ErrorKind::Parse(ref err) => write!(f, "Python parse error: {}", err),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum TranspileError {
    /// An error that occurred while transpiling the Python AST into Rust. A transform for this
    /// Python AST node was not implemented.
    Unimplemented {
        node: String,
        location: Option<Location>,
    },
}

impl TranspileError {
    pub fn unimplemented<D: fmt::Debug>(node: &D, location: Option<Location>) -> TranspileError {
        TranspileError::Unimplemented {
            node: format!("{:?}", node),
            location,
        }
    }
}

impl fmt::Display for TranspileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TranspileError::Unimplemented { node, location } => match location {
                // TODO: format location with {} / fmt::Display
                Some(loc) => write!(f, "Unimplemented node: {:?} at {:?}", node, loc),
                None => write!(f, "Unimplemented node: {:?}", node),
            },
        }
    }
}
