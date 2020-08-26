use rustc_ap_rustc_ast::ast;
use rustc_ap_rustc_ast::node_id;
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
