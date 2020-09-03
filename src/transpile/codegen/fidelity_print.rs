use crate::transpile::context::PrintContext;
use itertools::Itertools;
use log::warn;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ap_rustc_span::{source_map::Spanned, Symbol};
use rustc_ast::ast as rs;
use rustc_ast::ptr::P;
use rustc_ast::token;

/// Something that can be printed into Rust source code. Attempts to match
/// whatever original representation as closely as possible.
pub(crate) trait FidelityPrint {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String;
}

impl FidelityPrint for rs::UseTree {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
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
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.segments
            .iter()
            .map(|seg| seg.fidelity_print(ctx))
            .join("::")
    }
}

impl FidelityPrint for rs::PathSegment {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
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
    fn fidelity_print(&self, _ctx: &mut PrintContext) -> String {
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
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
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
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        let rs::FnHeader {
            unsafety,
            asyncness,
            constness,
            ext,
        } = self;

        let mut qualifiers = Vec::new();
        match unsafety {
            rs::Unsafe::Yes(_) => qualifiers.push("unsafe".to_string()),
            rs::Unsafe::No => {}
        }
        match asyncness {
            rs::Async::Yes { .. } => qualifiers.push("async".to_string()),
            rs::Async::No => {}
        }
        match constness {
            rs::Const::Yes(_) => qualifiers.push("const".to_string()),
            rs::Const::No => {}
        }
        match ext {
            rs::Extern::None => {}
            rs::Extern::Implicit => qualifiers.push("extern".to_string()),
            rs::Extern::Explicit(s) => {
                let s = format!("extern {}", s.fidelity_print(ctx));
                qualifiers.push(s)
            }
        }

        qualifiers.join(" ")
    }
}

impl FidelityPrint for Vec<rs::Param> {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.iter()
            .map(|param| param.fidelity_print(ctx))
            .join(", ")
    }
}

impl FidelityPrint for rs::Param {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        format!(
            "{}: {}",
            self.pat.fidelity_print(ctx),
            self.ty.fidelity_print(ctx)
        )
    }
}

impl FidelityPrint for rs::Pat {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match &self.kind {
            rs::PatKind::Ident(mode, ident, pat) => {
                if pat.is_some() {
                    unimplemented!()
                };

                let bmode = match mode {
                    rs::BindingMode::ByRef(rs::Mutability::Mut) => "ref mut ",
                    rs::BindingMode::ByRef(rs::Mutability::Not) => "ref ",
                    rs::BindingMode::ByValue(rs::Mutability::Mut) => "mut ",
                    rs::BindingMode::ByValue(rs::Mutability::Not) => "",
                };
                let binding = ident.fidelity_print(ctx);
                // ef. "ref mut binding"
                format!("{}{}", bmode, binding)
            }
            _ => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for rs::Ty {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match &self.kind {
            rs::TyKind::Slice(t) => format!("[{}]", t.fidelity_print(ctx)),
            rs::TyKind::Array(_, _) => ctx.unimplemented_print(self),
            rs::TyKind::Ptr(_) => ctx.unimplemented_print(self),
            rs::TyKind::Rptr(_, _) => ctx.unimplemented_print(self),
            rs::TyKind::BareFn(_) => ctx.unimplemented_print(self),
            rs::TyKind::Never => format!("!"),
            rs::TyKind::Tup(tuple) => {
                "(".to_owned() + &tuple.iter().map(|t| t.fidelity_print(ctx)).join(", ") + ")"
            }
            rs::TyKind::Path(_, _) => ctx.unimplemented_print(self),
            rs::TyKind::TraitObject(_, _) => ctx.unimplemented_print(self),
            rs::TyKind::ImplTrait(_, _) => ctx.unimplemented_print(self),
            rs::TyKind::Paren(_) => ctx.unimplemented_print(self),
            rs::TyKind::Typeof(_) => ctx.unimplemented_print(self),
            rs::TyKind::Infer => format!("_"),
            // FIXME: maybe Self? just a guess.
            rs::TyKind::ImplicitSelf => format!("Self"),
            rs::TyKind::MacCall(_) => ctx.unimplemented_print(self),
            rs::TyKind::Err => ctx.unimplemented_print(self),
            rs::TyKind::CVarArgs => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for rs::FnRetTy {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            // The default return for functions is rendered as `()`
            rs::FnRetTy::Default(_span) => format!("()"),
            rs::FnRetTy::Ty(ty_ptr) => {
                let ty = &ty_ptr.kind;
                // Don't render inferred return types
                if let rs::TyKind::Infer = ty {
                    "".to_owned()
                } else {
                    format!(" -> {}", ty_ptr.fidelity_print(ctx))
                }
            }
        }
    }
}

impl FidelityPrint for rs::Stmt {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.kind.fidelity_print(ctx)
    }
}

impl FidelityPrint for rs::StmtKind {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            rs::StmtKind::Local(local) => local.fidelity_print(ctx),
            rs::StmtKind::Item(_) => ctx.unimplemented_print(self),
            rs::StmtKind::Expr(expr) => expr.fidelity_print(ctx),
            // Semi adds a ';' at the end of the expression
            rs::StmtKind::Semi(expr) => format!("{};", expr.fidelity_print(ctx)),
            rs::StmtKind::Empty => format!(";"),
            rs::StmtKind::MacCall(_) => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for rs::StrLit {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.as_lit().fidelity_print(ctx)
    }
}

impl FidelityPrint for rs::Lit {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.token.fidelity_print(ctx)
    }
}

impl FidelityPrint for token::Lit {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self.kind {
            token::LitKind::Bool => ctx.unimplemented_print(&self.symbol),
            token::LitKind::Byte => ctx.unimplemented_print(&self.symbol),
            token::LitKind::Char => ctx.unimplemented_print(&self.symbol),
            token::LitKind::Integer => ctx.unimplemented_print(&self.symbol),
            token::LitKind::Float => ctx.unimplemented_print(&self.symbol),
            token::LitKind::Str => format!("\"{}\"", self.symbol.fidelity_print(ctx)),
            token::LitKind::StrRaw(_) => ctx.unimplemented_print(&self.symbol),
            token::LitKind::ByteStr => ctx.unimplemented_print(&self.symbol),
            token::LitKind::ByteStrRaw(_) => ctx.unimplemented_print(&self.symbol),
            token::LitKind::Err => ctx.unimplemented_print(&self.symbol),
        }
    }
}

impl FidelityPrint for Symbol {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.to_string()
    }
}

impl FidelityPrint for rs::Local {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        let rs::Local { pat, ty, init, .. } = self;

        let v = &pat.fidelity_print(ctx);

        let ty = match ty {
            Some(ty) => format!(": {}", ty.fidelity_print(ctx)),
            None => "".to_owned(),
        };

        let init = match init {
            Some(init) => format!(" = {}", init.fidelity_print(ctx)),
            None => "".to_owned(),
        };

        // `let v: Ty = init`
        format!("let {v}{ty}{init};", v = v, ty = ty, init = init)
    }
}

impl FidelityPrint for rs::Expr {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        if self.attrs.len() != 0 {
            ctx.unimplemented(&self.attrs);
        }
        self.kind.fidelity_print(ctx)
    }
}

impl FidelityPrint for rs::ExprKind {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            rs::ExprKind::Box(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Array(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Call(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::MethodCall(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Tup(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Binary(op, a, b) => format!(
                "{a} {op} {b}",
                op = op.fidelity_print(ctx),
                a = a.fidelity_print(ctx),
                b = b.fidelity_print(ctx)
            ),
            rs::ExprKind::Unary(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Lit(lit) => lit.fidelity_print(ctx),
            rs::ExprKind::Cast(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Type(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Let(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::If(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::While(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::ForLoop(_, _, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Loop(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Match(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Closure(_, _, _, _, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Block(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Async(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Await(_) => ctx.unimplemented_print(self),
            rs::ExprKind::TryBlock(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Assign(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::AssignOp(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Field(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Index(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Range(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Path(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::AddrOf(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Break(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Continue(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Ret(_) => ctx.unimplemented_print(self),
            rs::ExprKind::InlineAsm(_) => ctx.unimplemented_print(self),
            rs::ExprKind::LlvmInlineAsm(_) => ctx.unimplemented_print(self),
            rs::ExprKind::MacCall(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Struct(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Repeat(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Paren(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Try(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Yield(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Err => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for Spanned<rs::BinOpKind> {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.node.fidelity_print(ctx)
    }
}

impl FidelityPrint for rs::BinOpKind {
    fn fidelity_print(&self, _ctx: &mut PrintContext) -> String {
        let s = match self {
            rs::BinOpKind::Add => "+",
            rs::BinOpKind::Sub => "-",
            rs::BinOpKind::Mul => "*",
            rs::BinOpKind::Div => "/",
            rs::BinOpKind::Rem => "%",
            rs::BinOpKind::And => "&&",
            rs::BinOpKind::Or => "||",
            rs::BinOpKind::BitXor => "^",
            rs::BinOpKind::BitAnd => "&",
            rs::BinOpKind::BitOr => "|",
            rs::BinOpKind::Shl => "<<",
            rs::BinOpKind::Shr => ">>",
            rs::BinOpKind::Eq => "==",
            rs::BinOpKind::Lt => "<",
            rs::BinOpKind::Le => "<=",
            rs::BinOpKind::Ne => "!=",
            rs::BinOpKind::Ge => ">=",
            rs::BinOpKind::Gt => ">",
        };

        s.to_owned()
    }
}
