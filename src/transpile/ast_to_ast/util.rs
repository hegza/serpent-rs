use rustc_ap_rustc_ast::node_id;
use rustc_ap_rustc_span::{symbol, Span, DUMMY_SP};
use std::sync::Mutex;

/// Constructs an interned string for the identifier
pub fn ident(from: &str) -> symbol::Ident {
    let name = symbol::Symbol::intern(from);
    symbol::Ident::with_dummy_span(name)
}

/// Constructs a dummy node id using NodeId::MAX
pub fn node_id() -> node_id::NodeId {
    node_id::NodeId::MAX
}

/// Constructs a dummy span using DUMMY_SP
pub fn span() -> Span {
    DUMMY_SP
}
