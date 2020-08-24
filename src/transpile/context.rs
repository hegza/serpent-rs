use super::python;
use crate::{error::TranspileNodeError, transpile::rust};
use python::Node;
use rustpython_parser::{ast::Located, location::Location};
use std::fmt::Debug;

pub(crate) type RustAst = Vec<rust::NodeKind>;

/// Tracks the status of transpilation at a module level and provides reporting.
pub(crate) struct AstContext<'py_ast> {
    source_nodes: &'py_ast [python::NodeKind],
    /// Number of Python nodes transpiled thus far.
    node_idx: usize,
    /// The created, transpiled Rust AST nodes in order of creation.
    rust_nodes: Vec<rust::NodeKind>,
    unimplemented_handler: Box<dyn UnimplementedHandler>,
}

impl<'py_ast> AstContext<'py_ast> {
    pub fn new(runnable: bool, source_nodes: &[python::NodeKind]) -> AstContext {
        AstContext {
            source_nodes,
            node_idx: 0,
            rust_nodes: Vec::new(),
            unimplemented_handler: Box::new(ListUnimplemented::new()),
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

    /// Call to emit transpiled Rust source code from the transpiled Rust AST.
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

pub(crate) struct PrintContext {}

impl PrintContext {
    pub fn unimplemented<T>(&mut self, item: &T) -> String
    where
        T: Debug,
    {
        unimplemented!()
    }
}

trait UnimplementedHandler {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location);
    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    );
    fn report(&self) -> Result<(), TranspileNodeError>;
}

struct ListUnimplemented {
    items: Vec<String>,
    first: Option<Location>,
}

impl ListUnimplemented {
    fn new() -> ListUnimplemented {
        ListUnimplemented {
            items: vec![],
            first: None,
        }
    }
}

impl UnimplementedHandler for ListUnimplemented {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location) {
        self.items.push(format!("{}: {:?}", location, item));

        // Track the first untranspiled instance for returned error
        if self.first.is_none() {
            self.first = Some(location.clone());
        }
    }

    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    ) {
        self.items.push(format!(
            "{}: item `{}` for parameter {:?}",
            location, item_name, parameter
        ));

        // Track the first untranspiled instance for returned error
        if self.first.is_none() {
            self.first = Some(location.clone());
        }
    }

    fn report(&self) -> Result<(), TranspileNodeError> {
        if self.items.is_empty() {
            return Ok(());
        }

        println!("Unimplemented items:");
        for item in &self.items {
            println!("\t{}", item);
        }

        // Return the first untranspiled node as error
        Err(TranspileNodeError::Unimplemented {
            location: self.first.clone(),
            node: self.items.first().unwrap().clone(),
        })
    }
}
