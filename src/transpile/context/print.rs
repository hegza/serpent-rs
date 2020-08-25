use super::handler;
use crate::{error::ExpandError, transpile::config::TranspileConfig};
use std::fmt::Debug;

pub(crate) struct PrintContext {
    /// Number of nodes transpiled thus far
    idx: usize,
    unimplemented_handler: Box<dyn handler::UnimplementedExpand>,
    /// Produced Rust source code
    target: String,
    emit_placeholders: bool,
}

impl PrintContext {
    pub fn new(cfg: &TranspileConfig) -> PrintContext {
        let mut emit_placeholders = false;

        use crate::transpile::config::MissingImplBehavior;
        let unimplemented_handler: Box<dyn handler::UnimplementedExpand> = match cfg.on_missing_impl
        {
            MissingImplBehavior::EmitDummy => {
                emit_placeholders = true;
                Box::new(handler::WarnOnUnimplemented {})
            }
            MissingImplBehavior::Omit => Box::new(handler::WarnOnUnimplemented {}),
            MissingImplBehavior::ErrorAtAst | MissingImplBehavior::ErrorAtCodegen => {
                Box::new(handler::ListUnimplementedExpand::new())
            }
        };

        PrintContext {
            unimplemented_handler,
            idx: 0,
            target: String::new(),
            emit_placeholders,
        }
    }
    pub fn unimplemented<T>(&mut self, item: &T)
    where
        T: Debug,
    {
        if self.emit_placeholders {
            self.emit(&format!("// T-TODO: rs::{:?}", item));
        }
        self.unimplemented_handler.handle_unimplemented(&item);
    }

    pub fn advance(&mut self) {
        self.idx += 1;
    }

    /// Call to emit transpiled Rust source code.
    pub fn emit(&mut self, rust: &str) {
        self.target.push_str(rust);
    }

    /// Call to emit transpiled Rust source code from the transpiled Rust AST.
    pub fn finish(self) -> Result<String, ExpandError> {
        // Report AST-to-AST transpilation errors
        self.unimplemented_handler.report()?;

        Ok(self.target)
    }
}
