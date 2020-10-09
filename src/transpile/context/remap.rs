use std::collections::HashMap;

use rustpython_parser::ast as py;

/// Represents remap-related functionality in the ProgramContext.
pub(crate) struct RemapContext {
    /// We need to track Python use aliases to be able to apply remaps based on
    /// their original name.
    /// Key: alias, Value: original name
    pub use_aliases: HashMap<String, String>,
    /// We use this to detect whether we are inside a Python
    /// ExpressionType::Call. We only enable call-remapping within calls.
    call_depth: usize,
}

impl RemapContext {
    pub fn new() -> RemapContext {
        RemapContext {
            use_aliases: HashMap::new(),
            call_depth: 0,
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

    pub fn start_call(&mut self) {
        self.call_depth += 1;
    }
    pub fn end_call(&mut self) {
        self.call_depth -= 1;
    }
    fn is_within_call(&self) -> bool {
        self.call_depth != 0
    }
}
