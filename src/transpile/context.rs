mod error_strategy;
mod print;
mod remap;

pub(crate) use print::PrintContext;

use super::{ast_to_ast::dummy, python};
use crate::config::TranspileConfig;
use crate::{error::TranspileNodeError, transpile::rust, PyModule};
use error_strategy::*;
use log::{error, trace};
use python::Node;
use remap::RemapContext;
use rustc_ap_rustc_ast::ast as rs;
use rustc_ap_rustc_ast::ptr::P;
use rustpython_parser::ast as py;
use rustpython_parser::ast::Located;
use std::fmt::Debug;

pub(crate) type RustAst = Vec<rust::NodeKind>;

pub(crate) struct ProgramContext<'py_paths> {
    /// The module that is being transpiled
    source_module: &'py_paths PyModule,
    /// Number of Python files transpiled thus far
    file_idx: usize,
}

impl<'py_paths> ProgramContext<'py_paths> {
    pub fn new(source_module: &'py_paths PyModule) -> ProgramContext<'py_paths> {
        ProgramContext {
            file_idx: 0,
            source_module,
        }
    }

    pub fn source_module(&self) -> &'py_paths PyModule {
        self.source_module
    }

    pub fn advance(&mut self) {
        self.file_idx += 1;
    }
}

/// Tracks the status of transpilation at a module level and provides reporting.
pub(crate) struct AstContext<'py_ast> {
    /// The AST that is being transpiled
    source_nodes: &'py_ast [python::NodeKind],
    /// Number of Python nodes transpiled thus far.
    node_idx: usize,
    // Other local module symbols relative to the source module of this AST
    relative_mod_symbols: &'py_ast [String],
    unimplemented_handler: Box<dyn HandleUnimplementedAst>,
    emit_placeholders: bool,
    // `depth` and `block` help implement recursing into blocks
    depth: usize,
    /// The created, transpiled Rust AST nodes in order of creation, organized
    /// by current depth. Blocks get removed from this Vec after they're
    /// finished and attached to their top-level nodes.
    block_recurse: Vec<Vec<rust::NodeKind>>,
    remap_ctx: RemapContext,
}

impl<'py_ast> AstContext<'py_ast> {
    pub fn new(
        relative_mod_symbols: &'py_ast [String],
        source_nodes: &'py_ast [python::NodeKind],
        cfg: &TranspileConfig,
    ) -> AstContext<'py_ast> {
        let mut emit_placeholders = false;

        use crate::config::MissingImplBehavior;
        let unimplemented_handler: Box<dyn HandleUnimplementedAst> = match cfg.on_missing_impl {
            MissingImplBehavior::EmitDummy => {
                emit_placeholders = true;
                Box::new(WarnOnUnimplemented {})
            }
            MissingImplBehavior::Omit => Box::new(WarnOnUnimplemented {}),
            MissingImplBehavior::ErrorAtAst => Box::new(ListUnimplemented::new(false)),
            MissingImplBehavior::ErrorAtCodegen => Box::new(ListUnimplemented::new(true)),
            MissingImplBehavior::PanicImmediately => Box::new(AlwaysPanic {}),
        };

        AstContext {
            source_nodes,
            node_idx: 0,
            relative_mod_symbols,
            unimplemented_handler,
            emit_placeholders,
            depth: 0,
            block_recurse: vec![Vec::new()],
            remap_ctx: RemapContext::new(),
        }
    }

    /// Call this after finishing transpilation of a Python node.
    pub fn advance(&mut self) {
        self.node_idx += 1;
    }

    /// Call to emit a transpiled Rust AST node.
    pub fn emit(&mut self, rust_node: rust::NodeKind) {
        self.block_recurse[self.depth].push(rust_node);
    }

    pub fn detect_use_alias(&mut self, import_symbol: &py::ImportSymbol) {
        self.remap_ctx.detect_use_alias(import_symbol)
    }

    /// Recurse into a block, eg. in a function
    pub fn start_block(&mut self) {
        self.depth += 1;
        self.block_recurse.push(Vec::new());
    }

    /// Finish recursion into a block and return the block
    pub fn finish_block(&mut self) -> P<rs::Block> {
        self.depth -= 1;

        let nodes = self
            .block_recurse
            .pop()
            .expect("finish_block called without respective start_block")
            .into_iter();

        let mut stmts = Vec::with_capacity(nodes.len());
        for node in nodes {
            match node {
                rust::NodeKind::ExtendedStmt(stmt) => stmts.push(stmt),
                // Extend ordinary statements with dummy data to match the Stmt trait spec
                rust::NodeKind::Stmt(stmt) => stmts.push(dummy::stmt(stmt)),
                _ => self.unimplemented_parameter("block", "node", &node),
            }
        }

        let block = rs::Block {
            stmts,
            id: dummy::node_id(),
            rules: rs::BlockCheckMode::Default,
            span: dummy::span(),
        };
        P(block)
    }

    pub fn identify_import(&self, symbol: &str) -> ImportKind {
        if self.relative_mod_symbols.iter().any(|s| s == symbol) {
            trace!(
                "Resolved '{}' as a local symbol, local symbol table: {:?}",
                symbol,
                &self.relative_mod_symbols
            );
            ImportKind::Local
        } else {
            trace!(
                "Resolved '{}' as a foreign symbol, local symbol table: {:?}",
                symbol,
                &self.relative_mod_symbols
            );
            ImportKind::Foreign
        }
    }

    /// Call to emit transpiled Rust AST
    pub fn finish(self) -> Result<RustAst, TranspileNodeError> {
        // Report AST-to-AST transpilation errors
        self.unimplemented_handler.report()?;

        if self.depth != 0
            || self.block_recurse.len() != 1
            || self.depth + 1 != self.block_recurse.len()
        {
            Err(TranspileNodeError::UnresolvedBlock(self.depth))
        } else {
            Ok(self.block_recurse.into_iter().nth(0).unwrap())
        }
    }

    /// Notifies that the given `item` is not implemented.
    pub fn unimplemented_item<T>(&mut self, item: &Located<T>)
    where
        T: Debug,
    {
        if self.emit_placeholders {
            self.emit(rust::NodeKind::Comment(format!(" T-TODO: py::{:?}", item)));
        }
        self.unimplemented_handler
            .handle_unimplemented_item(&item.node, &item.location);
    }

    /// Notifies that the given `branch` of a visit_{item} is not implemented.
    pub fn unimplemented_parameter<T>(
        &mut self,
        item_name: &str,
        parameter_name: &str,
        parameter_value: &T,
    ) where
        T: Debug,
    {
        let location = self.current_source_node().location().clone();
        self.unimplemented_handler.handle_unimplemented_parameter(
            item_name,
            parameter_name,
            parameter_value,
            &location,
        );
    }

    /// Like `unimplemented_item`, but returns a wildcard pattern as placeholder
    pub fn unimplemented_pat<T>(&mut self, pat: &Located<T>) -> rs::PatKind
    where
        T: Debug,
    {
        self.unimplemented_item(pat);
        rs::PatKind::Wild
    }

    fn current_source_node(&self) -> &python::NodeKind {
        &self.source_nodes[self.node_idx]
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ImportKind {
    Local,
    Foreign,
}
