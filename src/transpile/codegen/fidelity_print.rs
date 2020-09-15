use crate::transpile::context::PrintContext;
use itertools::Itertools;
use log::warn;
use rustc_ap_rustc_ast as rustc_ast;
use rustc_ap_rustc_span::source_map::Spanned;
use rustc_ast::ast as rs;
use rustc_ast::ptr::P;
use rustc_ast::token;
use std::ops::Deref;

/// Something that can be printed into Rust source code. Attempts to match
/// whatever original representation as closely as possible.
pub(crate) trait FidelityPrint {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String;
    // Shorthand for `fidelity_print`
    fn fp(&self, ctx: &mut PrintContext) -> String {
        self.fidelity_print(ctx)
    }
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
        into_formatted_list(&self.segments, "::", ctx)
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
        into_formatted_list(self, ", ", ctx)
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
                "(".to_owned() + &into_formatted_list(&tuple, ", ", ctx) + ")"
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
        let sym = self.symbol.to_string();

        match self.kind {
            token::LitKind::Bool => format!("{}", sym),
            token::LitKind::Byte => format!("b\"{}\"", sym),
            token::LitKind::Char => format!("'{}'", sym),
            token::LitKind::Integer => format!("{}", sym),
            token::LitKind::Float => format!("{}", sym),
            token::LitKind::Str => format!("\"{}\"", sym),
            token::LitKind::StrRaw(_) => format!("r\"{}\"", sym),
            token::LitKind::ByteStr => format!("b\"{}\"", sym),
            token::LitKind::ByteStrRaw(_) => ctx.unimplemented_print(self),
            token::LitKind::Err => format!("Err( {:?} )", sym),
        }
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
            rs::ExprKind::Box(expr) => format!("box {}", expr.fidelity_print(ctx)),
            rs::ExprKind::Array(elems) => {
                let elems = into_formatted_list(elems, ", ", ctx);
                format!("[{}]", elems)
            }
            rs::ExprKind::Call(func, args) => {
                let args = into_formatted_list(args, ", ", ctx);
                format!(
                    "{func}({args})",
                    func = func.fidelity_print(ctx),
                    args = args
                )
            }
            // A call to a method with a receiver. Note that the first element of `args` is the
            // receiver object.
            rs::ExprKind::MethodCall(name_and_generics, args, _span) => {
                let receiver = match args.first() {
                    Some(arg) => arg,
                    None => {
                        ctx.unimplemented(self);
                        unimplemented!()
                    }
                };
                // Omit the receiver from args
                let args = &args[1..];

                let rs::PathSegment {
                    ident: fn_name,
                    args: generics,
                    id: _id,
                } = name_and_generics;

                match generics {
                    None => format!(
                        "{receiver}.{fn_name}({args})",
                        receiver = receiver.fidelity_print(ctx),
                        fn_name = fn_name.fidelity_print(ctx),
                        args = into_formatted_list(args, ", ", ctx)
                    ),
                    Some(generics) => format!(
                        "{receiver}.{fn_name}::{generics_str}({args})",
                        receiver = receiver.fidelity_print(ctx),
                        fn_name = fn_name.fidelity_print(ctx),
                        generics_str = generics.fidelity_print(ctx),
                        args = into_formatted_list(args, ", ", ctx)
                    ),
                }
            }
            rs::ExprKind::Tup(tuple) => format!("({})", into_formatted_list(tuple, ", ", ctx)),
            rs::ExprKind::Binary(op, a, b) => format!(
                // HACK: always add parentheses, because we currently can't trace tokens from
                // source.
                // TODO: could try to infer when parentheses are not required by looking at the
                // subtree for simple cases.
                "({a} {op} {b})",
                op = op.fidelity_print(ctx),
                a = a.fidelity_print(ctx),
                b = b.fidelity_print(ctx)
            ),
            rs::ExprKind::Unary(op, expr) => format!(
                // HACK: always add parentheses, because we currently can't trace tokens from
                // source.
                // TODO: could try to infer when parentheses are not required by looking at the
                // subtree for simple cases.
                "{}({})",
                op = op.fidelity_print(ctx),
                expr = expr.fidelity_print(ctx)
            ),
            rs::ExprKind::Lit(lit) => lit.fidelity_print(ctx),
            rs::ExprKind::Cast(expr, as_ty) => format!(
                "{} as {}",
                expr.fidelity_print(ctx),
                as_ty.fidelity_print(ctx)
            ),
            rs::ExprKind::Type(expr, ty) => {
                format!("{}: {}", expr.fidelity_print(ctx), ty.fidelity_print(ctx))
            }
            // Let binding **expression** that is only semantically allowed in if/while expressions,
            // eg. `if let 0 = x {}`
            rs::ExprKind::Let(binding, expr) => format!(
                "let {} = {}",
                binding.fidelity_print(ctx),
                expr.fidelity_print(ctx)
            ),
            rs::ExprKind::If(if_expr, block, else_block) => {
                warn!("fidelity_print.rs - `impl FidelityPrint for rs::ExprKind {{... match self ... rs::ExprKind::If` workaround block complexity by using context to carefully emit, then return an empty string");

                // HACK: workaround block complexity by using context to carefully emit, then
                // return an empty string
                let if_expr = format!("if {}", if_expr.fidelity_print(ctx));
                ctx.emit(&if_expr);

                // Hit a space between the if expr and the consequent block
                ctx.emit(" ");

                // Emit from '{' to '}'
                super::visit_block(block, ctx);

                if let Some(block) = else_block {
                    // Hit a newline between block and the else expression
                    ctx.emit("\n");
                    let s = block.fidelity_print(ctx);
                    ctx.emit(&format!("else {}", s));
                }

                "".to_owned()
            }
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
            rs::ExprKind::Field(object, field) => format!("{}.{}", object.fp(ctx), field.fp(ctx)),
            rs::ExprKind::Index(path, index) => format!(
                "{}[{}]",
                path.fidelity_print(ctx),
                index.fidelity_print(ctx)
            ),
            rs::ExprKind::Range(start, end, _) => {
                // Unwrap is safe, because all transpiled ranges have a start and an end.
                // Otherwise they would have been transpiled as Index.
                format!(
                    "[{}..{}]",
                    start.as_ref().unwrap().fidelity_print(ctx),
                    end.as_ref().unwrap().fidelity_print(ctx)
                )
            }
            rs::ExprKind::Path(qself, path) => match qself {
                Some(_) => ctx.unimplemented_print(self),
                None => path.fidelity_print(ctx),
            },
            rs::ExprKind::AddrOf(_, _, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Break(_, _) => ctx.unimplemented_print(self),
            rs::ExprKind::Continue(_) => ctx.unimplemented_print(self),
            rs::ExprKind::Ret(val) => match val {
                None => "return".to_owned(),
                Some(val) => format!("return {}", val.fidelity_print(ctx)),
            },
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

impl FidelityPrint for rs::GenericArgs {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            rs::GenericArgs::AngleBracketed(args) => args.fidelity_print(ctx),
            rs::GenericArgs::Parenthesized(args) => args.fidelity_print(ctx),
        }
    }
}

impl FidelityPrint for rs::AngleBracketedArgs {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        let args = into_formatted_list(&self.args, ", ", ctx);
        format!("<{}>", args)
    }
}

impl FidelityPrint for rs::AngleBracketedArg {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            rs::AngleBracketedArg::Arg(generic_arg) => generic_arg.fidelity_print(ctx),
            rs::AngleBracketedArg::Constraint(constraint) => constraint.fidelity_print(ctx),
        }
    }
}

impl FidelityPrint for rs::GenericArg {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            rs::GenericArg::Lifetime(lt) => lt.fidelity_print(ctx),
            rs::GenericArg::Type(ty) => ty.fidelity_print(ctx),
            rs::GenericArg::Const(anon_const) => anon_const.fidelity_print(ctx),
        }
    }
}

impl FidelityPrint for rs::Lifetime {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        // Add preceding apostrophe
        format!("'{}", self.ident.fidelity_print(ctx))
    }
}

impl FidelityPrint for rs::AnonConst {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.value.fidelity_print(ctx)
    }
}

impl FidelityPrint for rs::AssocTyConstraint {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match &self.kind {
            rs::AssocTyConstraintKind::Equality { ty } => format!(
                "{} = {}",
                self.ident.fidelity_print(ctx),
                ty.fidelity_print(ctx)
            ),
            rs::AssocTyConstraintKind::Bound { bounds } => format!(
                "{}: {}",
                self.ident.fidelity_print(ctx),
                into_formatted_list(&bounds, " + ", ctx)
            ),
        }
    }
}

impl FidelityPrint for rs::GenericBound {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        match self {
            rs::GenericBound::Trait(_, _) => ctx.unimplemented_print(self),
            rs::GenericBound::Outlives(_) => ctx.unimplemented_print(self),
        }
    }
}

impl FidelityPrint for rs::ParenthesizedArgs {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        let inputs = into_formatted_list(&self.inputs, ", ", ctx);
        let output = self.output.fidelity_print(ctx);

        format!("({}) -> {}", inputs, output)
    }
}

impl FidelityPrint for rs::UnOp {
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        let s = match self {
            rs::UnOp::Deref => "*",
            rs::UnOp::Not => "!",
            rs::UnOp::Neg => "-",
        };

        s.to_owned()
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

/// Blanket implementation for pointers to FidelityPrint types
impl<T> FidelityPrint for P<T>
where
    T: FidelityPrint,
{
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.deref().fidelity_print(ctx)
    }
}

/// Blanket implementation for Spanned versions of FidelityPrint types, because
/// we don't care about Spans.
impl<T> FidelityPrint for Spanned<T>
where
    T: FidelityPrint,
{
    fn fidelity_print(&self, ctx: &mut PrintContext) -> String {
        self.node.fidelity_print(ctx)
    }
}

fn into_formatted_list<T>(list: &[T], sep: &str, ctx: &mut PrintContext) -> String
where
    T: FidelityPrint,
{
    list.iter().map(|arg| arg.fidelity_print(ctx)).join(sep)
}
