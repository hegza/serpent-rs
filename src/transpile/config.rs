/// Changes the behavior of the transpiler.
pub struct TranspileConfig {
    /// What to do when an unimplemented AST-to-AST translation item is
    /// encountered.
    on_missing_impl: MissingImplBehavior,
}

#[derive(Clone, Debug)]
/// Describes behavior for what should happen when an unimplemented AST-to-AST
/// translation item is encountered.
pub enum MissingImplBehavior {
    /// Emit a placeholder that the user can correct post-config
    EmitDummy,
    /// Omit the missing item in the transpiled version
    Omit,
    /// Panic with unimplemented!()
    Panic,
}
