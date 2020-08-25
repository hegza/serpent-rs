use super::{
    config::TranspileConfig,
    context::{PrintContext, RustAst},
};
use crate::{error::ExpandError, transpile::rust};
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::ast as rs;

pub(crate) fn fidelity_print(ast: &RustAst, cfg: &TranspileConfig) -> Result<String, ExpandError> {
    // Construct a transpiled program by fidelity printing the Rust AST
    let mut ctx = PrintContext::new(cfg);
    for rust_node in ast {
        rust_node.fidelity_print(&mut ctx)?;
    }
    ctx.finish()
}

/// Something that can be printed into Rust source code. Attempts to match
/// whatever original representation as closely as possible.
pub(crate) trait FidelityPrint {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> Result<(), ExpandError>;
}

impl FidelityPrint for rust::NodeKind {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> Result<(), ExpandError> {
        use rust::NodeKind;

        match self {
            NodeKind::Item(item) => visit_item(item, ctx),
            NodeKind::Newline => ctx.emit("\n"),
            NodeKind::Comment(content) => ctx.emit(&format!("//{}", content)),
        }

        ctx.advance();
        Ok(())
    }
}

pub enum CodegenError {
    NotImplemented,
}

fn visit_item(item: &rs::ItemKind, ctx: &mut PrintContext) {
    match item {
        rs::ItemKind::ExternCrate(_) => ctx.unimplemented(item),
        rs::ItemKind::Use(_) => ctx.unimplemented(item),
        rs::ItemKind::Static(_, _, _) => ctx.unimplemented(item),
        rs::ItemKind::Const(_, _, _) => ctx.unimplemented(item),
        rs::ItemKind::Fn(_, _, _, _) => ctx.unimplemented(item),
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
