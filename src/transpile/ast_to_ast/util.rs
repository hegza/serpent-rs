use rustc_ap_rustc_ast::ast;
use rustc_ap_rustc_span::{symbol, Span, DUMMY_SP};

use super::dummy;

/// Constructs an interned string for the identifier
pub fn ident(from: &str) -> symbol::Ident {
    let name = symbol(from);
    symbol::Ident::with_dummy_span(name)
}

/// Constructs an interned string for the symbol
pub fn symbol(from: &str) -> symbol::Symbol {
    symbol::Symbol::intern(from)
}

/// Constructs a pattern from string
pub fn str_to_pat(from: &str) -> ast::Pat {
    ast::Pat {
        id: dummy::node_id(),
        kind: str_to_pat_kind(from),
        span: dummy::span(),
    }
}

pub fn str_to_pat_kind(from: &str) -> ast::PatKind {
    ast::PatKind::Ident(
        ast::BindingMode::ByValue(ast::Mutability::Not),
        ident(from),
        None,
    )
}

pub fn str_to_path(id: &str) -> ast::Path {
    let segments = id
        .split('.')
        .map(ident)
        .map(ast::PathSegment::from_ident)
        .collect::<Vec<ast::PathSegment>>();

    ast::Path {
        span: dummy::span(),
        segments,
    }
}
