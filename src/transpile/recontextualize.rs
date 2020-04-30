use super::PyNode;
use crate::error::{Error, ErrorKind, Result};
use crate::transpile::identify_lines::{identify_lines, LineKind};
use rustpython_parser::ast::*;
use std::error::Error as StdError;
use std::fmt;

/// Annotates the program with line kinds, returning fully contextualized nodes that can be used to
/// generate Rust.
pub(crate) fn recontextualize(src: &str, program: Program) -> Result<Vec<PyNode>> {
    let mut nodes = vec![];

    let mut parsed_statements = program.statements.into_iter();

    let line_kinds = identify_lines(src)?;
    for (line_no, ref line_kind) in line_kinds.iter().enumerate() {
        match line_kind {
            LineKind::Newline => nodes.push(PyNode::Newline(Located {
                location: Location::new(line_no, 0),
                node: (),
            })),
            LineKind::Comment(s) => nodes.push(PyNode::Comment(Located {
                location: Location::new(line_no, 0),
                node: s.to_owned(),
            })),
            // If it's the first line of a statement, extract the next statement from the parsed program
            LineKind::Statement(0) => {
                let stmt = parsed_statements.next();
                match stmt {
                    Some(stmt) => nodes.push(PyNode::Statement(stmt)),
                    None => {
                        return Err(Error::new(ErrorKind::Recontextualize(
                            RecontextualizeError::ParserDivergence,
                        )))
                    }
                }
            }
            // Ignore non-first lines related to a statement
            LineKind::Statement(_) => continue,
        }
    }

    Ok(nodes)
}

#[derive(Debug)]
pub enum RecontextualizeError {
    ParserDivergence,
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
        }
    }
}
