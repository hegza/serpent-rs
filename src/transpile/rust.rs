use rustc_ap_rustc_ast::ast::{Item, ItemKind, Stmt};

/// An item, a statement, a newline or a comment of Rust.
#[derive(Debug)]
pub(crate) enum NodeKind {
    /// An anonymous Rust AST item
    Item(ItemKind),
    /// A Rust AST item with attributes, ident, etc.
    ExtendedItem(Item),
    /// A Rust statement
    Stmt(Stmt),
    Newline,
    Comment(String),
}
