mod fidelity_print;

use super::{
    config::TranspileConfig,
    context::{PrintContext, RustAst},
};
use crate::{error::ExpandError, transpile::rust};
use fidelity_print::{FidelityPrint, FnSignature};
use itertools::Itertools;
use log::warn;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::ast as rs;
use rustc_ast::ptr::P;

pub(crate) fn ast_to_rust(ast: &RustAst, cfg: &TranspileConfig) -> Result<String, ExpandError> {
    // Construct a transpiled program by fidelity printing the Rust AST
    let mut ctx = PrintContext::new(cfg);
    for rust_node in ast {
        match rust_node {
            rust::NodeKind::Item(item) => visit_item(None, item, &mut ctx),
            rust::NodeKind::ExtendedItem(item) => visit_item_trait(item, &mut ctx),
            rust::NodeKind::Stmt(stmt) => visit_stmt(stmt, &mut ctx),
            rust::NodeKind::ExtendedStmt(stmt) => visit_stmt_trait(stmt, &mut ctx),
            rust::NodeKind::Newline => ctx.emit("\n"),
            rust::NodeKind::Comment(content) => ctx.emit(&format!("//{}", content)),
        }

        ctx.advance();
    }
    ctx.finish()
}

// <!-- Visitor implementations, code emission here -->

fn visit_item(name: Option<&str>, item: &rs::ItemKind, ctx: &mut PrintContext) {
    match item {
        rs::ItemKind::ExternCrate(_) => ctx.unimplemented(item),
        rs::ItemKind::Use(use_tree) => {
            let out_str = use_tree.fidelity_print(ctx);
            ctx.emit(&out_str);
        }
        rs::ItemKind::Static(_, _, _) => ctx.unimplemented(item),
        rs::ItemKind::Const(_, _, _) => ctx.unimplemented(item),
        rs::ItemKind::Fn(defaultness, fn_sig, generics, block) => visit_fn(
            name.expect("function must have a name"),
            defaultness,
            fn_sig,
            generics,
            block,
            ctx,
        ),
        rs::ItemKind::Mod(_) => ctx.unimplemented(item),
        rs::ItemKind::ForeignMod(_) => ctx.unimplemented(item),
        rs::ItemKind::GlobalAsm(_) => ctx.unimplemented(item),
        rs::ItemKind::TyAlias(_, _, _, _) => ctx.unimplemented(item),
        rs::ItemKind::Enum(_, _) => ctx.unimplemented(item),
        rs::ItemKind::Struct(_, _) => ctx.unimplemented(item),
        rs::ItemKind::Union(_, _) => ctx.unimplemented(item),
        rs::ItemKind::Trait(_, _, _, _, _) => ctx.unimplemented(item),
        rs::ItemKind::TraitAlias(_, _) => ctx.unimplemented(item),
        rs::ItemKind::Impl {
            unsafety,
            polarity,
            defaultness,
            constness,
            generics,
            of_trait,
            self_ty,
            items,
        } => ctx.unimplemented(item),
        rs::ItemKind::MacCall(_) => ctx.unimplemented(item),
        rs::ItemKind::MacroDef(_) => ctx.unimplemented(item),
    }
}

fn visit_item_trait(item: &rs::Item, ctx: &mut PrintContext) {
    let rs::Item {
        attrs,
        id,
        span,
        vis,
        /// The name of the item.
        /// It might be a dummy name in case of anonymous items.
        ident,

        kind,

        /// Original tokens this item was parsed from. This isn't necessarily
        /// available for all items, although over time more and more items
        /// should have this be `Some`. Right now this is primarily used
        /// for procedural macros, notably custom attributes.
        ///
        /// Note that the tokens here do not include the outer attributes, but
        /// will include inner attributes.
        tokens,
    } = item;

    visit_item(Some(&ident.to_string()), kind, ctx);
}

/// Visits a statement and emits it's contents
fn visit_stmt(stmt: &rs::StmtKind, ctx: &mut PrintContext) {
    let out_str = stmt.fidelity_print(ctx);
    ctx.emit(&out_str)
}

fn visit_stmt_trait(stmt: &rs::Stmt, ctx: &mut PrintContext) {
    let rs::Stmt { id, kind, span } = stmt;

    visit_stmt(kind, ctx);
}

fn visit_fn(
    name: &str,
    defaultness: &rs::Defaultness,
    fn_sig: &rs::FnSig,
    generics: &rs::Generics,
    block: &Option<P<rs::Block>>,
    ctx: &mut PrintContext,
) {
    // Emit signature
    let signature = FnSignature(name, fn_sig, generics);
    let out_str = signature.fidelity_print(ctx);
    ctx.emit(&out_str);

    if let Some(block) = block {
        // Hit a space between a function signature and it's block
        ctx.emit(" ");

        // Start a block
        ctx.start_block();

        // Emit statements in block
        for stmt in &block.stmts {
            let mut stmt_str = stmt.fidelity_print(ctx);
            // Add a newline to each statement
            stmt_str.push('\n');
            ctx.emit(&stmt_str);
        }

        // Finish block
        ctx.finish_block();

        // Hit a newline after finishing a function block
        ctx.emit("\n");
    }
    // no block
    else {
        ctx.emit(";");
    }
}
