use rustc_ap_rustc_ast::ast::ItemKind;

/// A statement, newline or a comment of Rust.
#[derive(Debug)]
pub(crate) enum NodeKind {
    Item(ItemKind),
    Newline,
    Comment(String),
}
