use crate::transpile::ast_to_ast::{from_py::FromPy, util};
use itertools::Itertools;
use log::{error, warn};
use rustc_ap_rustc_ast::ast as rs;
use rustc_ap_rustc_span::symbol;
use rustpython_parser::ast as py;
use toml::{map::Map as TomlMap, Value as TomlValue};

use std::collections::HashMap;

/// Represents remap-related functionality in the ProgramContext.
pub(crate) struct RemapContext {
    /// We need to track Python use aliases to be able to apply remaps based on
    /// their original name.
    /// Key: alias, Value: original name
    pub use_aliases: HashMap<String, String>,
    /// We use this to detect whether we are inside a Python
    /// ExpressionType::Call. We only enable call-remapping within calls.
    call_depth: usize,
    pub remaps: PathRemap,
}

impl RemapContext {
    pub fn new(remaps: Option<&TomlMap<String, TomlValue>>) -> RemapContext {
        // Get default entries
        let default_remaps = {
            let value = include_str!("DefaultRemaps.toml")
                .parse::<TomlValue>()
                .unwrap();
            if let TomlValue::Table(table) = value {
                table
            } else {
                panic!()
            }
        };

        let remaps = if let Some(remap) = remaps {
            let remaps = merge_maps(&default_remaps, remap);
            create_remaps(&remaps)
        } else {
            create_remaps(&default_remaps)
        };

        RemapContext {
            use_aliases: HashMap::new(),
            call_depth: 0,
            remaps,
        }
    }

    /// Call to detect and store a use alias for use in remapping.
    // HACK: Slight hack.
    pub fn detect_use_alias(&mut self, import_symbol: &py::ImportSymbol) {
        // De-structure import symbol
        let py::ImportSymbol { symbol, alias } = import_symbol;

        if let Some(alias) = alias {
            self.store_use_alias(symbol.clone(), alias.clone());
        }
    }
    fn store_use_alias(&mut self, name: String, alias: String) {
        self.use_aliases.insert(alias, name);
    }
    /// Un-aliases the given name. Returns the parameter itself if not aliased.
    fn resolve_alias(&self, name: &str) -> String {
        self.use_aliases
            .get(name)
            .unwrap_or(&name.to_string())
            .clone()
    }

    /// Checks if the given attribute is remapped and returns the remapped
    /// expression and field.
    pub fn get_remapped_attribute(
        &self,
        object: &py::ExpressionType,
        name: &str,
    ) -> Option<(rs::Expr, symbol::Ident)> {
        let mut path = self.convert_attribute_to_path(object, name);
        path[0] = self.resolve_alias(&path[0]).to_string();

        if let Some(replacement) = self.remaps.get(&path) {
            None
        } else {
            None
        }
    }
    fn convert_attribute_to_path(&self, object: &py::ExpressionType, name: &str) -> Vec<String> {
        match object {
            py::ExpressionType::Attribute { value, name: inner } => {
                let mut path = self.convert_attribute_to_path(&value.node, inner);
                path.push(name.to_owned());
                path
            }
            py::ExpressionType::Identifier { name: inner } => {
                // Unaliasing happens only in attribute, for object, when object is Identifier;
                // this might work?
                let unaliased = self.resolve_alias(inner);
                vec![unaliased, name.to_owned()]
            }
            _ => unimplemented!(),
        }
    }

    pub fn start_call(&mut self) {
        self.call_depth += 1;
    }
    pub fn end_call(&mut self) {
        self.call_depth -= 1;
    }
    fn is_within_call(&self) -> bool {
        self.call_depth != 0
    }

    pub fn remap_function(&self, function: &py::ExpressionType) -> Option<Vec<String>> {
        let path = match function {
            py::ExpressionType::Attribute { value, name } => {
                Some(self.convert_attribute_to_path(&value.node, name))
            }
            py::ExpressionType::Identifier { name } => Some(vec![self.resolve_alias(name)]),
            _ => None,
        };

        if let Some(path) = path {
            // Check if a remap exists
            if let Some(replacement_template) = self.remaps.get(&path) {
                Some(replacement_template.to_vec())
            } else {
                None
            }
        } else {
            None
        }
    }
    /// Returns the remapped symbol
    pub fn remap_symbol(&self, py_symbol: &str) -> Option<String> {
        if let Some(replacement_templates) = self.remaps.get(&vec![py_symbol.to_owned()]) {
            for template in replacement_templates {
                let mut iter = template.split_whitespace();
                let first_word = iter.nth(0);
                if let Some(first_word) = first_word {
                    match first_word {
                        // HACK: only replace symbols of type "crate"
                        "crate" => {
                            let rest = iter.join(" ");
                            return Some(rest);
                        }
                        _ => warn!("symbols will not be remapped if target is not 'crate'"),
                    }
                } else {
                    warn!(
                        "remap found for symbol {:?} but target string is empty",
                        py_symbol
                    )
                }
            }

            return None;
        } else {
            None
        }
    }
}

type PathRemap = HashMap<Vec<String>, Vec<String>>;

fn create_remaps(source: &TomlMap<String, TomlValue>) -> PathRemap {
    let mut remaps = PathRemap::new();

    for (key, value) in source {
        let path = Vec::new();
        remap_recurse(key, value, &path, &mut remaps);
    }

    remaps
}

fn remap_recurse(key: &str, value: &TomlValue, path: &[String], remaps: &mut PathRemap) {
    match key {
        "remap" => {
            // Remap current path
            if let TomlValue::String(template_str) = value {
                remap(path, template_str, remaps);
            } else {
                panic!("node should be a template string");
            };
        }
        "push_parameter" => {
            // Add parameter to target
            if let TomlValue::String(template_str) = value {
                remap(path, &("push_parameter ".to_owned() + template_str), remaps);
            }
        }
        inner => {
            // Add path and recurse deeper
            let mut path = path.to_vec();
            path.push(inner.to_owned());
            if let TomlValue::Table(table) = value {
                for (key, value) in table {
                    remap_recurse(key, value, &path, remaps);
                }
            } else {
                panic!(
                    "unidentified key {:?} should be table but: {:?}",
                    key, value
                )
            }
        }
    }
}

fn remap(path: &[String], target: &str, remaps: &mut PathRemap) {
    let entry = remaps.get_mut(path);
    if let Some(value) = entry {
        value.push(target.to_owned());
    } else {
        remaps.insert(path.to_vec(), vec![target.to_owned()]);
    }
}

fn merge_maps(
    a: &TomlMap<String, TomlValue>,
    b: &TomlMap<String, TomlValue>,
) -> TomlMap<String, TomlValue> {
    let mut nmap = TomlMap::new();

    for (k, v) in a.iter().chain(b.iter()) {
        nmap.insert(k.clone(), v.clone());
    }

    nmap
}
