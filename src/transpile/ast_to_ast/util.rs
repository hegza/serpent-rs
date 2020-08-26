use rustc_ap_rustc_ast::ast;
use rustc_ap_rustc_span::{symbol, Span, DUMMY_SP};

use super::dummy;

/// Constructs an interned string for the identifier
pub fn ident(from: &str) -> symbol::Ident {
    let name = symbol::Symbol::intern(from);
    symbol::Ident::with_dummy_span(name)
}

/// Constructs a pattern from string
pub fn str_to_pat(from: &str) -> ast::Pat {
    let pat = ast::PatKind::Ident(
        ast::BindingMode::ByValue(ast::Mutability::Not),
        ident(from),
        None,
    );
    ast::Pat {
        id: dummy::node_id(),
        kind: pat,
        span: dummy::span(),
    }
}
