use super::context::{PrintContext, RustAst};
use crate::config::TranspileConfig;
use crate::{error::ExpandError, transpile::rust};
use rustc_ap_rustc_ast_pretty::pprust;

pub(crate) fn ast_to_rust(ast: &RustAst, cfg: &TranspileConfig) -> Result<String, ExpandError> {
    // Construct a transpiled program by fidelity printing the Rust AST
    let mut ctx = PrintContext::new(cfg);
    for rust_node in ast {
        let rust = match rust_node {
            rust::NodeKind::Item(item) => {
                pprust::item_to_string(&crate::transpile::ast_to_ast::dummy::item(
                    crate::transpile::ast_to_ast::util::ident(""),
                    item.clone(),
                ))
            }
            rust::NodeKind::ExtendedItem(item) => pprust::item_to_string(item),
            rust::NodeKind::Stmt(stmt) => {
                pprust::stmt_to_string(&crate::transpile::ast_to_ast::dummy::stmt(stmt.clone()))
            }
            rust::NodeKind::ExtendedStmt(stmt) => pprust::stmt_to_string(stmt),
            rust::NodeKind::Newline => "".to_string(),
            rust::NodeKind::Comment(content) => format!("//{}", content),
        };

        ctx.emit(&format!("{}\n", rust));

        ctx.advance();
    }
    ctx.finish()
}
