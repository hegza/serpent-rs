use super::error_strategy::*;
use crate::config::TranspileConfig;
use crate::error::ExpandError;

pub(crate) struct PrintContext {
    /// Number of nodes transpiled thus far
    idx: usize,
    unimplemented_handler: Box<dyn HandleUnimplementedExpand>,
    /// Produced Rust source code
    target: String,
    depth: usize,
}

impl PrintContext {
    pub fn new(cfg: &TranspileConfig) -> PrintContext {
        use crate::config::MissingImplBehavior;
        let unimplemented_handler: Box<dyn HandleUnimplementedExpand> = match cfg.on_missing_impl {
            MissingImplBehavior::EmitDummy => Box::new(WarnOnUnimplemented {}),
            MissingImplBehavior::Omit => Box::new(WarnOnUnimplemented {}),
            MissingImplBehavior::ErrorAtAst | MissingImplBehavior::ErrorAtCodegen => {
                Box::new(ListUnimplementedExpand::new())
            }
            MissingImplBehavior::PanicImmediately => Box::new(AlwaysPanic {}),
        };

        PrintContext {
            unimplemented_handler,
            idx: 0,
            target: String::new(),
            depth: 0,
        }
    }

    pub fn advance(&mut self) {
        self.idx += 1;
    }

    /// Call to emit transpiled Rust source code.
    pub fn emit(&mut self, rust: &str) {
        if self.depth == 0 {
            self.target.push_str(rust);
        } else {
            // Chain indentations and then add item
            self.target.push_str(
                &std::iter::repeat("    ")
                    .take(self.depth)
                    .chain(std::iter::once(rust))
                    .collect::<String>(),
            );
        }
    }

    /// Call to emit transpiled Rust source code from the transpiled Rust AST.
    pub fn finish(self) -> Result<String, ExpandError> {
        // Report AST-to-AST transpilation errors
        self.unimplemented_handler.report()?;

        Ok(self.target)
    }
}
