use std::{iter::FromIterator, str::FromStr};

use crate::transpile::parser_ext::{parse_comments, parse_orphan_newlines};
use crate::ApiError;
use itertools::Itertools;
use rustpython_parser::{ast, ast::Located, parser as py_parser};
use rustpython_ast::Location;

#[derive(Debug)]
pub struct PythonAst(pub(crate) Vec<NodeKind>);

/// A statement, newline or comment of Python with a start location.
/// Everything that's required to create an expression in Rust.
#[derive(PartialEq, Debug)]
pub enum NodeKind {
    Statement(Located<ast::StatementType>),
    Newline(Location),
    Comment(Located<String>),
}

impl From<ast::Statement> for NodeKind {
    fn from(stmt: ast::Statement) -> Self {
        NodeKind::Statement(stmt)
    }
}

pub trait Node {
    fn location(&self) -> &Location;
}

impl Node for NodeKind {
    fn location(&self) -> &Location {
        match &self {
            NodeKind::Statement(located) => &located.location,
            NodeKind::Newline(location) => location,
            NodeKind::Comment(located) => &located.location,
        }
    }
}

impl FromStr for PythonAst {
    type Err = ApiError;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        // Parse Python source into a Python AST using RustPython
        let py_ast = py_parser::parse_program(src)?;
        let stmt_nodes = py_ast
            .statements
            .into_iter()
            .map(|stmt| NodeKind::from(stmt));

        // Parse comments with their locations
        let comments = parse_comments(src);
        let comment_nodes = comments
            .into_iter()
            .map(|comment| NodeKind::Comment(comment));

        // Parse orphan newlines with their locations
        let newlines = parse_orphan_newlines(src);
        let newline_nodes = newlines
            .into_iter()
            .map(|row| NodeKind::Newline(Location::new(row, 1)));

        // Re-arrange comments and statements
        let py_nodes = stmt_nodes
            .merge_by(comment_nodes, |a, b| {
                let (a_loc, b_loc) = (a.location(), b.location());
                compare_locations(a_loc, b_loc)
            })
            .merge_by(newline_nodes, |a, b| {
                let (a_loc, b_loc) = (a.location(), b.location());
                compare_locations(a_loc, b_loc)
            })
            .collect::<PythonAst>();

        Ok(py_nodes)
    }
}

impl FromIterator<NodeKind> for PythonAst {
    fn from_iter<I: IntoIterator<Item = NodeKind>>(iter: I) -> Self {
        let mut nodes = Vec::new();

        for node in iter {
            nodes.push(node);
        }

        PythonAst(nodes)
    }
}

fn compare_locations(a_loc: &Location, b_loc: &Location) -> bool {
    if a_loc.row() < b_loc.row() {
        true
    } else if a_loc.row() == b_loc.row() {
        a_loc.column() < b_loc.column()
    } else {
        false
    }
}
