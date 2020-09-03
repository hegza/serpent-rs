/// Changes the behavior of the transpiler.
#[derive(Clone, Debug)]
pub struct TranspileConfig {
    /// What to do when an unimplemented AST-to-AST translation item is
    /// encountered.
    pub on_missing_impl: MissingImplBehavior,
}

/// Describes behavior for what should happen when an unimplemented AST-to-AST
/// translation item is encountered.
#[derive(Clone, Debug)]
pub enum MissingImplBehavior {
    /// Emit a placeholder based on original source that the user can correct
    EmitDummy,
    /// Omit the missing item in the transpiled version, but report a warning
    /// with warn!()
    Omit,
    /// Collect unimplemented Python AST -> Rust AST items as errors and return
    /// the first one
    ErrorAtAst,
    /// Collect unimplemented Rust AST -> Rust source items as errors and return
    /// the first one
    ErrorAtCodegen,
    /// Panic at untranspiled item, to get a call stack for sure.
    PanicImmediately,
}

impl Default for TranspileConfig {
    fn default() -> Self {
        TranspileConfig {
            on_missing_impl: MissingImplBehavior::ErrorAtCodegen,
        }
    }
}
