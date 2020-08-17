use std::borrow::{Borrow, BorrowMut};

use log::warn;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{punctuated::Punctuated, *};

pub(crate) fn remap_functions(stmt: &mut Stmt) {
    visit_stmt(stmt);
}

fn visit_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Local(local) => visit_local(local),
        Stmt::Item(item) => visit_item(item),
        Stmt::Expr(expr) => visit_expr(expr),
        Stmt::Semi(expr, _) => visit_expr(expr),
    }
}

// Visit a local like `let x: u64 = expr`. Remaps functions in the `expr` part.
fn visit_local(local: &mut Local) {
    if let Some((_, expr)) = &mut local.init {
        visit_expr(expr.borrow_mut());
    }
}

fn visit_item(item: &mut Item) {
    match item {
        //Item::Const(item_const) => visit_const(item_const),
        Item::Enum(_) => {}
        Item::ExternCrate(_) => {}
        Item::Fn(item_fn) => visit_fn(item_fn),
        //Item::Impl(item_impl) => visit_impl(item_impl),
        //Item::Mod(item_mod) => visit_mod(item_mod),
        //Item::Static(item_static) => visit_static(item_statiC),
        Item::Struct(_) => {}
        //Item::Trait(item_trait) => visit_trait(),
        Item::TraitAlias(_) => {}
        Item::Type(_) => {}
        Item::Union(_) => {}
        Item::Use(_) => {}
        item => {
            warn!("remap_functions::visit_item skips item `{:?}` as is has not been decided whether it can contain a function call", item);
        }
    }
}

// Visit the statements in the block of the function definition
fn visit_fn(item_fn: &mut ItemFn) {
    let block: &mut Block = item_fn.block.borrow_mut();
    for stmt in &mut block.stmts {
        visit_stmt(stmt);
    }
}

fn visit_expr(expr: &mut Expr) {
    // TODO: incomplete
    match expr {
        //Expr::Assign(assign) => visit_assign(),
        Expr::Binary(binary) => visit_binary(binary),
        Expr::Call(call) => match visit_call(call) {
            Some(replacement) => *expr = replacement,
            None => {}
        },
        Expr::Lit(_) => {}
        Expr::MethodCall(method_call) => visit_method_call(method_call),
        Expr::Path(_) => {}
        expr => {
            warn!(
                "remap_functions::visit_expr skips expression `{:?}` as it has not been implemented",
                expr
            );
        }
    }
}

// Visit both sides of the binary expression
fn visit_binary(binary: &mut ExprBinary) {
    visit_expr(binary.left.borrow_mut());
    visit_expr(binary.right.borrow_mut());
}

// TODO: apply function mappings properly
// Returns a replacing expression, if applicable
fn visit_call(call: &ExprCall) -> Option<Expr> {
    // Find the function name part from the call, eg. 'foo' in `foo(1, 2)`
    match call.func.borrow() {
        Expr::Path(path) => {
            let seg = path
                .path
                .segments
                .first()
                .expect("path in call should have at least one element");
            let ident = seg.ident.to_string();

            // Just map print here for now
            match ident.as_ref() {
                "print" => return Some(Expr::Macro(substitute_print(call))),
                _ => {}
            }
        }
        func => {
            unimplemented!("remap_functions::visit_call found a non-`Expr::Path` name for a function: `{:?}`, don't know what to do", func);
        }
    }
    None
}

fn visit_method_call(_call: &mut ExprMethodCall) {
    // TODO: no replacements implemented yet, though this function is probably where most work should be done in the future
}

/// Substitutes a "print(<x>)" of type  `ExprCall` with a "println!(<y>)" of type `ExprMacro`
fn substitute_print(call: &ExprCall) -> ExprMacro {
    let mut segments = Punctuated::new();
    segments.push(PathSegment {
        ident: Ident::new("println", Span::call_site()),
        arguments: PathArguments::None,
    });
    let path = Path {
        leading_colon: None,
        segments,
    };
    let println_macro = Macro {
        path,
        bang_token: <Token![!]>::default(),
        delimiter: MacroDelimiter::Paren(token::Paren {
            span: Span::call_site(),
        }),
        tokens: substitute_print_params(&call.args),
    };
    let expr = ExprMacro {
        attrs: vec![],
        mac: println_macro,
    };
    return expr;
}

fn substitute_print_params(args: &Punctuated<Expr, Token![,]>) -> TokenStream {
    let param_count = args.len();

    let format_str = "\"".to_string()
        + &std::iter::repeat("{}")
            .take(param_count)
            .collect::<Vec<&str>>()
            .join(" ")
        + "\"";

    let mut ret = args.clone();

    ret.insert(
        0,
        parse_str::<Expr>(&format_str).expect(&format!("cannot parse {}", &format_str)),
    );
    ret.to_token_stream()
}
