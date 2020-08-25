use crate::error::{ExpandError, TranspileNodeError};
use log::warn;
use rustpython_parser::location::Location;
use std::fmt::Debug;

pub trait UnimplementedAstNode {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location);
    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    );
    fn report(&self) -> Result<(), TranspileNodeError>;
}

/// Handler for missing fidelity print implementations
pub trait UnimplementedExpand {
    fn handle_unimplemented(&mut self, item: &dyn Debug);
    fn report(&self) -> Result<(), ExpandError>;
}

pub struct ListUnimplemented {
    items: Vec<String>,
    first: Option<Location>,
}

impl ListUnimplemented {
    pub fn new() -> ListUnimplemented {
        ListUnimplemented {
            items: vec![],
            first: None,
        }
    }
}

impl UnimplementedAstNode for ListUnimplemented {
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
            debug: self.items.first().unwrap().clone(),
        })
    }
}

pub struct WarnOnUnimplemented {}

impl UnimplementedAstNode for WarnOnUnimplemented {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location) {
        warn!("Unimplemented on {}: {:?}", location, item);
    }

    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    ) {
        warn!(
            "Unimplemented on {}: item `{}` for parameter {:?}",
            location, item_name, parameter,
        );
    }

    fn report(&self) -> Result<(), TranspileNodeError> {
        Ok(())
    }
}

impl UnimplementedExpand for WarnOnUnimplemented {
    fn handle_unimplemented(&mut self, item: &dyn Debug) {
        warn!("Unimplemented: {:?}", item);
    }

    fn report(&self) -> Result<(), ExpandError> {
        Ok(())
    }
}

pub struct ListUnimplementedExpand(Vec<String>);

impl ListUnimplementedExpand {
    pub fn new() -> ListUnimplementedExpand {
        ListUnimplementedExpand(vec![])
    }
}

impl UnimplementedExpand for ListUnimplementedExpand {
    fn handle_unimplemented(&mut self, item: &dyn Debug) {
        self.0.push(format!("{:?}", item));
    }

    fn report(&self) -> Result<(), ExpandError> {
        if self.0.is_empty() {
            return Ok(());
        }

        println!("Unimplemented expansion items:");
        for item in &self.0 {
            println!("\t{}", item);
        }

        // Return the first untranspiled node as error
        Err(ExpandError::Unimplemented(self.0.first().unwrap().clone()))
    }
}
