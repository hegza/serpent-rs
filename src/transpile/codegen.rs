use super::{
    config::TranspileConfig,
    context::{PrintContext, RustAst},
};
use crate::{error::ExpandError, transpile::rust};
use itertools::Itertools;
use log::warn;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::ast as rs;

pub(crate) fn ast_to_rust(ast: &RustAst, cfg: &TranspileConfig) -> Result<String, ExpandError> {
    // Construct a transpiled program by fidelity printing the Rust AST
    let mut ctx = PrintContext::new(cfg);
    for rust_node in ast {
        use rust::NodeKind;

        match rust_node {
            NodeKind::Item(item) => visit_item(item, &mut ctx),
            NodeKind::Newline => ctx.emit("\n"),
            NodeKind::Comment(content) => ctx.emit(&format!("//{}", content)),
        }

        ctx.advance();
    }
    ctx.finish()
}

// <!-- Visitor implementations, code emission here -->

fn visit_item(item: &rs::ItemKind, ctx: &mut PrintContext) {
    match item {
        rs::ItemKind::ExternCrate(_) => ctx.unimplemented(item),
        rs::ItemKind::Use(use_tree) => ctx.emit(&use_tree.fidelity_print(ctx)),
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

// <!-- formatting and print implementations here -->

/// Something that can be printed into Rust source code. Attempts to match
/// whatever original representation as closely as possible.
pub(crate) trait FidelityPrint {
    fn fidelity_print(&self, ctx: &PrintContext) -> String;
}

impl FidelityPrint for rs::UseTree {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        let prefix = self.prefix.fidelity_print(ctx);

        let path = match self.kind {
            rs::UseTreeKind::Simple(alias, _, _) => match alias {
                None => format!("{prefix}", prefix = prefix),
                Some(alias) => format!("{prefix} as {alias}", prefix = prefix, alias = alias),
            },
            rs::UseTreeKind::Nested(_) => ctx.unimplemented_print(&self.kind),
            rs::UseTreeKind::Glob => format!("{prefix}::*", prefix = prefix),
        };

        format!("use {};\n", path)
    }
}

impl FidelityPrint for rs::Path {
    // Join paths like `seg::seg`
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        self.segments
            .iter()
            .map(|seg| seg.fidelity_print(ctx))
            .join("::")
    }
}

impl FidelityPrint for rs::PathSegment {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        // Type / lifetime parameters attached to the path
        let tl_parameters = &self.args;
        if tl_parameters.is_some() {
            warn!(
                "Time / lifetime parameters not implemented (at node: {:?})",
                self
            )
        }

        self.ident.fidelity_print(ctx)
    }
}

impl FidelityPrint for rustc_ap_rustc_span::symbol::Ident {
    // NOTE: needs to lock the string interner
    fn fidelity_print(&self, _ctx: &PrintContext) -> String {
        self.to_string()
    }
}
