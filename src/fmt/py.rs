use std::fmt::Debug;

use crate::fmt::AstString;
use crate::transpile::python;
use itertools::Itertools;
use py::Located;
use rustpython_parser::ast as py;
use rustpython_parser::location::Location;

const SHOW_LOCATION: bool = false;

impl<'a> AstString for &Vec<python::NodeKind> {
    fn to_ast_string(&self) -> String {
        let elems = self.iter().map(|elem| elem.to_ast_string()).join(", ");
        format!("[{}]", elems)
    }
}

impl AstString for python::NodeKind {
    fn to_ast_string(&self) -> String {
        match self {
            python::NodeKind::Statement(stmt) => {
                format!("Statement {{ {} }}", stmt.to_ast_string())
            }
            python::NodeKind::Newline(loc) => format!("Newline {:?}", loc.to_ast_string()),
            python::NodeKind::Comment(s) => {
                format!("Comment {} {{ {:?} }}", s.location.to_ast_string(), s.node)
            }
        }
    }
}

impl AstString for Location {
    fn to_ast_string(&self) -> String {
        format!("[{}, {}]", self.row(), self.column())
    }
}

impl<T> AstString for Located<T>
where
    T: AstString,
{
    fn to_ast_string(&self) -> String {
        if SHOW_LOCATION {
            // Prefix the debug print with the location
            let loc = self.location;
            format!("{} {}", loc.to_ast_string(), self.node.to_ast_string())
        } else {
            // Hide the location from the debug print
            format!("{}", self.node.to_ast_string())
        }
    }
}

impl AstString for py::StatementType {}
