//! A minimal lexer to add contextual information from the Python source, not
//! directly supported by RustPython.
//!
//! Parses a Python source, contextualizing lines as belonging to a statement, a
//! comment, or something else. It's kind of hacky, extensions to RustPython
//! might make this redundant.

use std::iter::Enumerate;
use std::str::Lines;

use crate::error::{Result, TranspileError};
use rustpython_parser::parser::{parse_program, parse_statement};
use thiserror::Error as ThisError;

/// Parses a Python source into a list of line-kind identifiers, one for each
/// line.
pub(crate) fn identify_lines(src: &str) -> Result<Vec<(LineKind, String)>> {
    // Verify that source parses, heavy operation: debug only
    debug_assert!(parse_program(src).is_ok());

    let mut lines = src.lines().enumerate();
    let line_count = lines.clone().count();

    // Go over all lines in the source, and describe the lines in terms of their
    // type
    let mut line_kinds = Vec::with_capacity(line_count);
    while let Some((idx, line)) = lines.next() {
        let line = line.to_owned();
        let line_no = idx + 1;
        if is_blank_line(&line) {
            line_kinds.push((LineKind::Newline, line));
        } else if is_comment(&line) {
            line_kinds.push((LineKind::Comment(line.to_owned()), line));
        } else if starts_multiline_comment(&line) {
            let com_lines = consume_multiline_comment(line_no, line, &mut lines)?;
            line_kinds.extend(com_lines);
        }
        // Identify the line as a statement, if it's not whitespace or a comment
        else {
            let stmt_lines = consume_multiline_statement(line_no, line, &mut lines)?;
            line_kinds.extend(stmt_lines);
        }
    }

    debug_assert_eq!(line_kinds.len(), line_count);

    Ok(line_kinds)
}

fn consume_multiline_comment(
    first_line_no: usize,
    first_line: String,
    lines: &mut Enumerate<Lines>,
) -> Result<Vec<(LineKind, String)>> {
    let mut line_kinds = vec![];

    let mut len_lines = 1;
    let mut com_constituents = vec![first_line];

    // Loop, adding a line to the comment candidate each time until the end of the multiline-comment
    while let Some((_line_no, next)) = lines.next() {
        len_lines += 1;
        com_constituents.push(next.to_owned());

        // When end tag is found, return the structure with the comment lines
        if ends_multiline_comment(&next) {
            for n in 0..len_lines {
                line_kinds.push((
                    LineKind::MultilineComment(n, com_constituents[n].to_string()),
                    com_constituents[n].to_string(),
                ));
            }
            return Ok(line_kinds);
        }
    }

    // Return error if the iterator runs out without an ending comment tag ('"""')
    Err(TranspileError::IdentifyLines(
        IdentifyLinesError::EofWhileConstructingComment(first_line_no),
    ))
}

fn consume_multiline_statement(
    first_line_no: usize,
    first_line: String,
    lines: &mut Enumerate<Lines>,
) -> Result<Vec<(LineKind, String)>> {
    let mut line_kinds = vec![];

    let mut len_lines = 1;
    let mut stmt_constituents = vec![first_line];
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
                    Some(next) => stmt_constituents.push(next.1.to_owned()),
                    None => {
                        return Err(TranspileError::IdentifyLines(
                            IdentifyLinesError::EofWhileConstructingStatement(first_line_no),
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

    Ok(line_kinds)
}

/// A line starts a multiline comment if its first three non-whitespace characters are '"""'
fn starts_multiline_comment(line: &str) -> bool {
    let trimmed = line.trim();

    // Need at least 3 characters for '"""'
    if trimmed.len() < 3 {
        return false;
    }

    &trimmed[0..3] == "\"\"\""
}

/// A line ends a multiline comment if its last three non-whitespace characters are '"""'
fn ends_multiline_comment(line: &str) -> bool {
    let trimmed = line.trim();

    // Need at least 3 characters for '"""'
    if trimmed.len() < 3 {
        return false;
    }

    &trimmed[trimmed.len() - 3..trimmed.len()] == "\"\"\""
}

fn is_comment(line: &str) -> bool {
    match line.trim().chars().nth(0) {
        Some('#') => true,
        _ => false,
    }
}

fn is_blank_line(line: &str) -> bool {
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
    MultilineComment(usize, String),
    Newline,
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Context;
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

\"\"\"
This is a multiline comment
\"\"\"
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
            LineKind::Newline,
            LineKind::MultilineComment(0, "\"\"\"".to_owned()),
            LineKind::MultilineComment(1, "This is a multiline comment".to_owned()),
            LineKind::MultilineComment(2, "\"\"\"".to_owned()),
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
        for (idx, (left, right)) in identify_lines(PYTHON_SOURCE_2)
            .context("identify lines error")
            .unwrap()
            .into_iter()
            .map(|(kind, _line)| kind)
            .zip(PYTHON_SOURCE_2_IDENTIFIED.iter())
            .enumerate()
        {
            assert_eq!(&left, right, "at line {}", idx + 1);
        }
    }
}

#[derive(ThisError, Debug)]
pub enum IdentifyLinesError {
    #[error("EOF while attempting to construct a multiline statement starting from line {0}")]
    EofWhileConstructingStatement(usize),
    #[error("EOF while attempting to construct a multiline comment starting from line {0}")]
    EofWhileConstructingComment(usize),
}
