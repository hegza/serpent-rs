use super::util;
use rustc_ap_rustc_ast::ast;
use rustc_ap_rustc_ast::{node_id, token};
use rustc_ap_rustc_data_structures::thin_vec::ThinVec;
use rustc_ap_rustc_span::{Span, DUMMY_SP};

/// Constructs a dummy node id using NodeId::MAX
pub fn node_id() -> node_id::NodeId {
    node_id::NodeId::MAX
}

/// Constructs a dummy span using DUMMY_SP
pub fn span() -> Span {
    DUMMY_SP
}

pub fn attr_vec() -> ast::AttrVec {
    ThinVec::new()
}

pub fn token(kind: ast::LitKind) -> token::Lit {
    kind.to_lit_token()
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
        kind: kind,
        span: span(),
    }
}

pub fn block(stmts: Vec<ast::Stmt>) -> ast::Block {
    ast::Block {
        stmts,
        id: node_id(),
        rules: ast::BlockCheckMode::Default,
        span: span(),
    }
}
