use super::context::{PrintContext, RustAst};
use crate::transpile::rust::NodeKind;
use rustc_ap_rustc_ast::ast::ItemKind;

pub(crate) fn fidelity_print(ast: &RustAst) -> String {
    let mut out_str = String::new();

    // Construct a transpiled program by fidelity printing the Rust AST
    let mut ctx = PrintContext {};
    for rust_node in ast {
        let formatted = rust_node.fidelity_print(&mut ctx);

        out_str.push_str(&formatted);
    }

    out_str
}

/// Something that can be printed into Rust source code. Attempts to match
/// whatever original representation as closely as possible.
pub(crate) trait FidelityPrint {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String;
}

impl FidelityPrint for NodeKind {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            NodeKind::Item(item) => item.fidelity_print(ctx),
            NodeKind::Newline => "\n".to_owned(),
            NodeKind::Comment(content) => format!("//{}", content),
        }
    }
}

pub enum CodegenError {
    NotImplemented,
}

impl FidelityPrint for ItemKind {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            ItemKind::ExternCrate(_) => ctx.unimplemented(self),
            ItemKind::Use(_) => ctx.unimplemented(self),
            ItemKind::Static(_, _, _) => ctx.unimplemented(self),
            ItemKind::Const(_, _, _) => ctx.unimplemented(self),
            ItemKind::Fn(_, _, _, _) => ctx.unimplemented(self),
            ItemKind::Mod(_) => ctx.unimplemented(self),
            ItemKind::ForeignMod(_) => ctx.unimplemented(self),
            ItemKind::GlobalAsm(_) => ctx.unimplemented(self),
            ItemKind::TyAlias(_, _, _, _) => ctx.unimplemented(self),
            ItemKind::Enum(_, _) => ctx.unimplemented(self),
            ItemKind::Struct(_, _) => ctx.unimplemented(self),
            ItemKind::Union(_, _) => ctx.unimplemented(self),
            ItemKind::Trait(_, _, _, _, _) => ctx.unimplemented(self),
            ItemKind::TraitAlias(_, _) => ctx.unimplemented(self),
            ItemKind::Impl {
                unsafety,
                polarity,
                defaultness,
                constness,
                generics,
                of_trait,
                self_ty,
                items,
            } => ctx.unimplemented(self),
            ItemKind::MacCall(_) => ctx.unimplemented(self),
            ItemKind::MacroDef(_) => ctx.unimplemented(self),
        }
    }
}

// TODO: use this
// `impl ast::visit::Visitor` contains all variants `visit(&mut self, /**/)`
// &mut self could be used to produce code,
// or to visit a particular kind of AST node to eg. re-map libraries
