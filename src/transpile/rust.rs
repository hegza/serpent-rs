use rustc_ap_rustc_ast::ast::{Item, ItemKind, Stmt, StmtKind};

/// An item, a statement, a newline or a comment of Rust.
#[derive(Debug)]
pub(crate) enum NodeKind {
    /// An anonymous Rust AST item matching with rustc_ast::ItemKind.
    Item(ItemKind),
    /// A Rust AST item with attributes, ident, etc. matching with
    /// rustc_ast::Item.
    ExtendedItem(Item),
    /// A Rust statement matching with rustc_ast::StmtKind.
    Stmt(StmtKind),
    /// A Rust statement with attributes, ident, etc. matching with
    /// rustc_ast::Stmt.
    ExtendedStmt(Stmt),
    Newline,
    Comment(String),
}
