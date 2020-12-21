//! Edits at Rust AST level
use super::{ast_to_ast::dummy, context::RustAst};
use super::{ast_to_ast::util, rust};
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::ast as rs;

pub fn insert_crate_local_mods(rust_ast: &mut RustAst, mod_symbols: &[String]) {
    for sym in mod_symbols {
        if sym != "__init__" {
            // Create the mod definition
            let node = create_mod_node(sym);

            // Insert at start
            rust_ast.insert(0, node);
        }
    }
}

fn create_mod_node(mod_name: &str) -> rust::NodeKind {
    let mod_node = rs::Mod {
        inner: dummy::span(),
        items: vec![],
        inline: false,
        unsafety: rs::Unsafe::No,
    };

    let rust_item = rs::ItemKind::Mod(mod_node);
    let rust_item = dummy::item(util::ident(mod_name), rust_item);
    let rust_node = rust::NodeKind::ExtendedItem(rust_item);

    rust_node
}
