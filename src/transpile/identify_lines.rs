//! A minimal lexer to add contextual information from the Python source, not
//! directly supported by RustPython.
//!
//! Parses a Python source, contextualizing lines as belonging to a statement, a
//! comment, or something else. It's kind of hacky, extensions to RustPython
//! might make this redundant.

use crate::error::{Result, TranspileError};
use rustpython_parser::parser::{parse_program, parse_statement};
use thiserror::Error as ThisError;

/// Parses a Python source into a list of line-kind identifiers, one for each
/// line.
pub(crate) fn identify_lines(src: &str) -> Result<Vec<(LineKind, String)>> {
    // Verify that source parses
    debug_assert!(parse_program(src).is_ok());

    let mut lines = src.lines();
    let line_count = lines.clone().count();

    // Go over all lines in the source, and describe the lines in terms of their
    // type
    let mut line_kinds = Vec::with_capacity(line_count);
    while let Some(line) = lines.next() {
        let line = line.to_owned();
        if is_newline(&line) {
            line_kinds.push((LineKind::Newline, line));
        } else if is_comment(&line) {
            line_kinds.push((LineKind::Comment(line.to_owned()), line));
        }
        // Identify the line as a statement, if it's not whitespace or a comment
        else {
            let mut len_lines = 1;
            let mut stmt_constituents = vec![line];
            // Loop, adding a line to the statement candidate each time. The number of
            // iterations until a valid statement can be constructed determines the length
            // of the statement
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
                        len_lines += 1;
                        match next {
                            Some(next) => stmt_constituents.push(next.to_owned()),
                            None => {
                                return Err(TranspileError::IdentifyLines(
                                    IdentifyLinesError::EofWhileConstructingStatement,
                                ))
                            }
                        }
                        continue;
                    }
                }
            }
            for n in 0..len_lines {
                line_kinds.push((LineKind::Statement(n), stmt_constituents[n].to_string()));
            }
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
/// Represents the type of a a single line of Python. This is used to
/// contextualize a Python file.
pub(crate) enum LineKind {
    /// A line representing a statement. Inner parameter tells whether this is
    /// the first, second, etc. line of the statement for multiline
    /// statements.
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
            &identify_lines(PYTHON_SOURCE)
                .unwrap()
                .into_iter()
                .map(|(kind, _line)| kind)
                .collect::<Vec<LineKind>>(),
            &PYTHON_SOURCE_IDENTIFIED.to_vec()
        );
        assert_eq!(
            &identify_lines(PYTHON_SOURCE_2)
                .unwrap()
                .into_iter()
                .map(|(kind, _line)| kind)
                .collect::<Vec<LineKind>>(),
            &PYTHON_SOURCE_2_IDENTIFIED.to_vec()
        );
    }
}

#[derive(ThisError, Debug)]
pub enum IdentifyLinesError {
    #[error("EOF while attempting to construct a multiline statement by iterating over lines")]
    EofWhileConstructingStatement,
}
