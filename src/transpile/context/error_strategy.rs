use crate::error::{ExpandError, TranspileNodeError};
use log::warn;
use rustpython_parser::location::Location;
use std::fmt::Debug;

pub trait HandleUnimplementedAst {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location);
    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    );
    fn report(&self) -> Result<(), TranspileNodeError>;
}

/// Handler for missing fidelity print implementations
pub trait HandleUnimplementedExpand {
    fn handle_unimplemented(&mut self, item: &dyn Debug);
    fn report(&self) -> Result<(), ExpandError>;
}

#[derive(PartialEq)]
pub struct ListUnimplemented {
    items: Vec<String>,
    first: Option<Location>,
    allow_errors: bool,
}

impl ListUnimplemented {
    pub fn new(allow_errors: bool) -> ListUnimplemented {
        ListUnimplemented {
            items: vec![],
            first: None,
            allow_errors,
        }
    }
}

impl HandleUnimplementedAst for ListUnimplemented {
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
        parameter_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    ) {
        self.items.push(format!(
            "{}: parameter `{}` = {:?} is not implemented for item `{}`",
            location, parameter_name, parameter, item_name
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

        println!("Unimplemented Python -> Rust items:");
        for item in &self.items {
            println!("\t{}", item);
        }
        println!("Returning with {} errors.", self.items.len());

        // Return the first untranspiled node as error
        if self.allow_errors {
            Ok(())
        } else {
            Err(TranspileNodeError::Unimplemented {
                location: self.first.clone(),
                debug: self.items.first().unwrap().clone(),
            })
        }
    }
}

#[derive(PartialEq)]
pub struct WarnOnUnimplemented {}

impl HandleUnimplementedAst for WarnOnUnimplemented {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location) {
        warn!("Unimplemented on {}: {:?}", location, item);
    }

    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    ) {
        warn!(
            "Unimplemented on {}: item `{}` for parameter `{} = {:?}`",
            location, item_name, parameter_name, parameter,
        );
    }

    fn report(&self) -> Result<(), TranspileNodeError> {
        Ok(())
    }
}

impl HandleUnimplementedExpand for WarnOnUnimplemented {
    fn handle_unimplemented(&mut self, item: &dyn Debug) {
        warn!("Unimplemented: {:?}", item);
    }

    fn report(&self) -> Result<(), ExpandError> {
        Ok(())
    }
}

#[derive(PartialEq)]
pub struct ListUnimplementedExpand(Vec<String>);

impl ListUnimplementedExpand {
    pub fn new() -> ListUnimplementedExpand {
        ListUnimplementedExpand(vec![])
    }
}

impl HandleUnimplementedExpand for ListUnimplementedExpand {
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
        println!("Returning with {} errors.", self.0.len());

        // Return the first untranspiled node as error
        Err(ExpandError::Unimplemented(self.0.first().unwrap().clone()))
    }
}

#[derive(PartialEq)]
pub struct AlwaysPanic {}

impl HandleUnimplementedExpand for AlwaysPanic {
    fn handle_unimplemented(&mut self, item: &dyn Debug) {
        panic!("Unimplemented: {:?}", item);
    }

    fn report(&self) -> Result<(), ExpandError> {
        // If there were errors, we wouldn't get this far, thus it's always OK
        // to return OK.
        Ok(())
    }
}

impl HandleUnimplementedAst for AlwaysPanic {
    fn handle_unimplemented_item(&mut self, item: &dyn Debug, location: &Location) {
        panic!("Unimplemented on {}: {:?}", location, item);
    }

    fn handle_unimplemented_parameter(
        &mut self,
        item_name: &str,
        parameter_name: &str,
        parameter: &dyn Debug,
        location: &Location,
    ) {
        panic!(
            "Unimplemented on {}: item `{}` for parameter `{} = with value {:?}`",
            location, item_name, parameter_name, parameter,
        );
    }

    fn report(&self) -> Result<(), TranspileNodeError> {
        // If there were errors, we wouldn't get this far, thus it's always OK
        // to return OK.
        Ok(())
    }
}
