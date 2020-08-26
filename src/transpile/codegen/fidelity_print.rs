use crate::transpile::{
    config::TranspileConfig,
    context::{PrintContext, RustAst},
};
use crate::{error::ExpandError, transpile::rust};
use itertools::Itertools;
use log::warn;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ast::ast as rs;
use rustc_ast::ptr::P;

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

pub struct FnSignature<'a>(
    // Name,
    pub &'a str,
    // Signature
    pub &'a rs::FnSig,
    // Generics
    pub &'a rs::Generics,
);

impl<'a> FidelityPrint for FnSignature<'a> {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        let header = self.1.header.fidelity_print(ctx);
        let params = self.1.decl.inputs.fidelity_print(ctx);
        let ret_opt = self.1.decl.output.fidelity_print(ctx);

        // Create a full `fn` signature
        format!(
            "{header}fn {name}({params}){ret_opt}",
            header = header,
            name = self.0,
            params = params,
            ret_opt = ret_opt,
        )
    }
}

impl FidelityPrint for rs::FnHeader {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        let rs::FnHeader {
            unsafety,
            asyncness,
            constness,
            ext,
        } = self;

        let mut qualifiers = Vec::new();
        match unsafety {
            rs::Unsafe::Yes(_) => qualifiers.push("unsafe"),
            rs::Unsafe::No => {}
        }
        match asyncness {
            rs::Async::Yes { .. } => qualifiers.push("async"),
            rs::Async::No => {}
        }
        match constness {
            rs::Const::Yes(_) => qualifiers.push("const"),
            rs::Const::No => {}
        }

        qualifiers.join(" ")
    }
}

impl FidelityPrint for Vec<rs::Param> {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        self.iter()
            .map(|param| param.fidelity_print(ctx))
            .join(", ")
    }
}

impl FidelityPrint for rs::Param {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        format!(
            "{}: {}",
            self.pat.fidelity_print(ctx),
            self.ty.fidelity_print(ctx)
        )
    }
}

impl FidelityPrint for rs::Pat {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        match &self.kind {
            rs::PatKind::Ident(mode, ident, pat) => ident.fidelity_print(ctx),
            _ => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for rs::Ty {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        match &self.kind {
            _ => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for rs::FnRetTy {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        match self {
            // The default return for functions is rendered as `()`
            rs::FnRetTy::Default(_) => format!("()"),
            rs::FnRetTy::Ty(ty) => ty.fidelity_print(ctx),
        }
    }
}

impl FidelityPrint for rs::Stmt {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        self.kind.fidelity_print(ctx)
    }
}

impl FidelityPrint for rs::StmtKind {
    fn fidelity_print(&self, ctx: &PrintContext) -> String {
        match self {
            rs::StmtKind::Local(_) => ctx.unimplemented_print(self),
            rs::StmtKind::Item(_) => ctx.unimplemented_print(self),
            rs::StmtKind::Expr(_) => ctx.unimplemented_print(self),
            rs::StmtKind::Semi(_) => ctx.unimplemented_print(self),
            rs::StmtKind::Empty => ctx.unimplemented_print(self),
            rs::StmtKind::MacCall(_) => ctx.unimplemented_print(self),
        }
    }
}
