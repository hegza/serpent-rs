mod handler;
mod print;

pub(crate) use print::PrintContext;

use super::{config::TranspileConfig, python};
use crate::{error::TranspileNodeError, transpile::rust, PyModule};
use log::trace;
use python::Node;
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
    unimplemented_handler: Box<dyn handler::UnimplementedAstNode>,
    /// The created, transpiled Rust AST nodes in order of creation.
    rust_nodes: Vec<rust::NodeKind>,
    emit_placeholders: bool,
}

impl<'py_ast> AstContext<'py_ast> {
    pub fn new(
        relative_mod_symbols: &'py_ast [String],
        source_nodes: &'py_ast [python::NodeKind],
        cfg: &TranspileConfig,
    ) -> AstContext<'py_ast> {
        let mut emit_placeholders = false;

        use super::config::MissingImplBehavior;
        let unimplemented_handler: Box<dyn handler::UnimplementedAstNode> = match cfg
            .on_missing_impl
        {
            MissingImplBehavior::EmitDummy => {
                emit_placeholders = true;
                Box::new(handler::WarnOnUnimplemented {})
            }
            MissingImplBehavior::Omit => Box::new(handler::WarnOnUnimplemented {}),
            MissingImplBehavior::ErrorAtAst => Box::new(handler::ListUnimplemented::new(false)),
            MissingImplBehavior::ErrorAtCodegen => Box::new(handler::ListUnimplemented::new(true)),
        };

        AstContext {
            source_nodes,
            node_idx: 0,
            rust_nodes: Vec::new(),
            relative_mod_symbols,
            unimplemented_handler,
            emit_placeholders,
        }
    }

    /// Call this after finishing transpilation of a Python node.
    pub fn advance(&mut self) {
        self.node_idx += 1;
    }

    /// Call to emit a transpiled Rust AST node.
    pub fn emit(&mut self, rust_node: rust::NodeKind) {
        self.rust_nodes.push(rust_node);
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

        Ok(self.rust_nodes)
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

    /// Notifies that the given `branch` of an item is not implemented.
    pub fn unimplemented_parameter<T>(&mut self, item_name: &str, parameter: &T)
    where
        T: Debug,
    {
        let location = self.current_source_node().location().clone();
        self.unimplemented_handler
            .handle_unimplemented_parameter(item_name, parameter, &location);
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
