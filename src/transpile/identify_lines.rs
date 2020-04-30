//! A minimal lexer to add contextual information from the Python source, not directly supported by
//! RustPython.
//!
//! Parses a Python source, contextualizing lines as belonging to a statement, a comment, or
//! something else. It's kind of hacky, extensions to RustPython might make this redundant.

use crate::error::{Error, ErrorKind, Result};
use rustpython_parser::parser::{parse_program, parse_statement};
use std::error::Error as StdError;
use std::fmt;

/// Parses a Python source into a list of line-kind identifiers.
pub(crate) fn identify_lines(src: &str) -> Result<Vec<LineKind>> {
    // Verify that source parses
    debug_assert!(parse_program(src).is_ok());

    let mut lines = src.lines();
    let line_count = lines.clone().count();

    let mut line_kinds = Vec::with_capacity(line_count);
    loop {
        if let Some(line) = lines.next() {
            if is_newline(line) {
                line_kinds.push(LineKind::Newline);
            } else if is_comment(line) {
                line_kinds.push(LineKind::Comment(line.to_owned()));
            }
            // Identify the line as a statement, if it's not whitespace or a comment
            else {
                let mut len = 1;
                let mut stmt_constituents = vec![line];
                // Loop, adding a line to the statement candidate each time. The number of iterations until a valid statement can be constructed determines the length of the statement
                loop {
                    let aggregate = stmt_constituents
                        .iter()
                        .fold(String::new(), |agg, item| agg + item);

                    let stmt = parse_statement(&aggregate);
                    match stmt {
                        Ok(_) => break,
                        Err(_) => {
                            // Get the next line, returning error if the file ends.
                            let next = lines.next();
                            len += 1;
                            match next {
                                Some(next) => stmt_constituents.push(next),
                                None => {
                                    return Err(Error::new(ErrorKind::IdentifyLines(
                                        IdentifyLinesError::EofWhileConstructingStatement,
                                    )))
                                }
                            }
                            continue;
                        }
                    }
                }
                for n in 0..len {
                    line_kinds.push(LineKind::Statement(n));
                }
            }
        } else {
            break;
        }
    }

    debug_assert_eq!(line_kinds.len(), line_count);

    Ok(line_kinds)
}

fn is_comment(line: &str) -> bool {
    match line.trim().chars().nth(0) {
        Some('#') => true,
        _ => false,
    }
}

fn is_newline(line: &str) -> bool {
    line.trim().is_empty()
}

#[derive(Clone, Debug, PartialEq)]
/// Represents the type of a a single line of Python. This is used to contextualize a Python file.
pub(crate) enum LineKind {
    /// A line representing a statement. Inner parameter tells whether this is the first, second,
    /// etc. line of the statement for multiline statements.
    Statement(usize),
    Comment(String),
    Newline,
}

#[cfg(test)]
mod test {
    use super::*;

    use lazy_static::lazy_static;

    const PYTHON_SOURCE: &str = "\
# This is a function
def add(a, b):
    return a + b

# This is a variable
c = 3
# Here's a print statement
print(add(2 + c))
";

    const PYTHON_SOURCE_2: &str = "\
# This is a function
def add(a, b):
    return a + b

# This is a variable
c = 3
# Here's a print statement
print(
    add(2 + c)
)
";

    lazy_static! {
        static ref PYTHON_SOURCE_IDENTIFIED: Vec<LineKind> = vec![
            LineKind::Comment("# This is a function".to_string()),
            LineKind::Statement(0),
            LineKind::Statement(1),
            LineKind::Newline,
            LineKind::Comment("# This is a variable".to_owned()),
            LineKind::Statement(0),
            LineKind::Comment("# Here's a print statement".to_owned()),
            LineKind::Statement(0),
        ];
        static ref PYTHON_SOURCE_2_IDENTIFIED: Vec<LineKind> = vec![
            LineKind::Comment("# This is a function".to_owned()),
            LineKind::Statement(0),
            LineKind::Statement(1),
            LineKind::Newline,
            LineKind::Comment("# This is a variable".to_owned()),
            LineKind::Statement(0),
            LineKind::Comment("# Here's a print statement".to_owned()),
            LineKind::Statement(0),
            LineKind::Statement(1),
            LineKind::Statement(2),
        ];
    }

    #[test]
    fn lines_are_identified() {
        assert_eq!(
            &identify_lines(PYTHON_SOURCE).unwrap(),
            &PYTHON_SOURCE_IDENTIFIED.to_vec()
        );
        assert_eq!(
            &identify_lines(PYTHON_SOURCE_2).unwrap(),
            &PYTHON_SOURCE_2_IDENTIFIED.to_vec()
        );
    }
}

#[derive(Debug)]
pub enum IdentifyLinesError {
    EofWhileConstructingStatement,
}

impl StdError for IdentifyLinesError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        None
    }
}

impl fmt::Display for IdentifyLinesError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IdentifyLinesError::EofWhileConstructingStatement => write!(
                f,
                "Got EOF while trying to construct a statement by iterating over lines."
            ),
        }
    }
}
