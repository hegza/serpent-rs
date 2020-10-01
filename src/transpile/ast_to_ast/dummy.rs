//! Methods that add placeholder data to complete the requested type.
use rustc_ap_rustc_ast::ast;
use rustc_ap_rustc_ast::node_id;
use rustc_ap_rustc_data_structures::thin_vec::ThinVec;
use rustc_ap_rustc_span::{symbol, Span, DUMMY_SP};

/// Constructs a dummy node id using NodeId::MAX
pub fn node_id() -> node_id::NodeId {
    // HACK: NodeId::MAX is should be the dummy id, but 0 takes less screen space
    // for debugging
    unsafe { node_id::NodeId::from_u32_unchecked(0) }
}

/// Constructs a dummy span using DUMMY_SP
pub fn span() -> Span {
    DUMMY_SP
}

pub fn attr_vec() -> ast::AttrVec {
    ThinVec::new()
}

pub fn expr(kind: ast::ExprKind) -> ast::Expr {
    ast::Expr {
        id: node_id(),
        kind,
        span: span(),
        attrs: attr_vec(),
        // TODO:
        tokens: None,
    }
}

pub fn stmt(kind: ast::StmtKind) -> ast::Stmt {
    ast::Stmt {
        id: node_id(),
        kind,
        span: span(),
    }
}

pub fn item(ident: symbol::Ident, kind: ast::ItemKind) -> ast::Item {
    ast::Item {
        attrs: vec![],
        id: node_id(),
        span: span(),
        vis: ast::Visibility {
            node: ast::VisibilityKind::Public,
            span: span(),
        },
        ident,
        kind: kind,
        tokens: None,
    }
}
