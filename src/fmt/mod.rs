mod py;
mod rs;

use std::fmt;

/// A custom `Display` trait for AST nodes. Allows us to print AST nodes for
/// user-facing applications eg. a CLI.
pub trait AstString: fmt::Debug {
    fn to_ast_string(&self) -> String {
        // Use debug print by default
        format!("{:?}", self)
    }
}
