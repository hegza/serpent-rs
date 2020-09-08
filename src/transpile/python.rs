use rustpython_parser::{ast, ast::Located, location::Location};

/// A statement, newline or comment of Python with a start location.
/// Everything that's required to create an expression in Rust.
#[derive(Debug)]
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
