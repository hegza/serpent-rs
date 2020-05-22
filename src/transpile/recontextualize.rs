use super::{PyNode, PyNodeKind};
use crate::error::{Result, TranspileError};
use crate::transpile::identify_lines::{identify_lines, LineKind};
use rustpython_parser::ast::*;
use std::error::Error as StdError;
use std::fmt;

/// Annotates the program with line kinds, returning contextualized nodes that
/// can be used to generate Rust. Nodes don't quite match to lines, eg. a
/// function block is just one node.
pub(crate) fn recontextualize(src: &str, program: Program) -> Result<Vec<PyNode>> {
    let mut nodes = vec![];

    let mut parsed_statements = program.statements.into_iter();

    let line_kinds = identify_lines(src)?;
    let mut line_kinds = line_kinds.into_iter().enumerate().peekable();
    while let Some((line_no, (ref line_kind, ref line))) = line_kinds.next() {
        match line_kind {
            LineKind::Newline => nodes.push(PyNode::new(
                line.to_string(),
                PyNodeKind::Newline(Located {
                    location: Location::new(line_no, 0),
                    node: (),
                }),
            )),
            LineKind::Comment(s) => nodes.push(PyNode::new(
                line.to_string(),
                PyNodeKind::Comment(Located {
                    location: Location::new(line_no, 0),
                    node: s.to_owned(),
                }),
            )),
            // If it's the first line of a statement, extract the next statement from the parsed
            // program
            LineKind::Statement(0) => {
                let stmt = parsed_statements.next();
                let mut full_line = String::from(line.to_string());
                while match line_kinds.peek() {
                    Some(&(_, (LineKind::Statement(x), _))) if x != 0 => true,
                    _ => false,
                } {
                    let line = (line_kinds.next().unwrap().1).1;
                    full_line.push_str(&line);
                }

                match stmt {
                    Some(stmt) => nodes.push(PyNode::new(
                        full_line.to_string(),
                        PyNodeKind::Statement(stmt),
                    )),
                    None => {
                        return Err(TranspileError::Recontextualize(
                            RecontextualizeError::ParserDivergence,
                        ))
                    }
                }
            }
            // Non-first lines should be handled by the above implementation
            LineKind::Statement(n) => {
                return Err(TranspileError::Recontextualize(
                    RecontextualizeError::MultilineNotHandled(line_no, *n),
                ))
            }
        }
    }

    Ok(nodes)
}

#[derive(Debug)]
pub enum RecontextualizeError {
    ParserDivergence,
    // Parameters are `source line`, and index of the line of multiline statement
    MultilineNotHandled(usize, usize),
}

impl StdError for RecontextualizeError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        None
    }
}

impl fmt::Display for RecontextualizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RecontextualizeError::ParserDivergence => write!(
                f,
                "Parsed source has a different number of statements from what were identified by custom lexer in identify_lines."
            ),
            RecontextualizeError::MultilineNotHandled(line_no, idx) => write!(
                f,
                "A multiline statement on line {} was not fully captured by an expression that attempted to capture it. Multiline index: {}", line_no, idx
            ),
        }
    }
}
