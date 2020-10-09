use toml::{map::Map as TomlMap, value::Value as TomlValue};

/// Changes the behavior of the transpiler.
#[derive(Clone, PartialEq, Debug)]
pub struct TranspileConfig {
    /// What to do when an unimplemented AST-to-AST translation item is
    /// encountered.
    pub on_missing_impl: MissingImplBehavior,
    pub infer_options: Vec<InferOption>,
    pub extra_dependencies: Option<TomlMap<String, TomlValue>>,
    pub remap: Option<TomlMap<String, TomlValue>>,
}

impl Default for TranspileConfig {
    fn default() -> Self {
        TranspileConfig {
            on_missing_impl: MissingImplBehavior::Omit,
            infer_options: vec![InferOption::InitFileIntoMainRs, InferOption::InitFnIntoMain],
            extra_dependencies: None,
            remap: None,
        }
    }
}

/// Describes behavior for what should happen when an unimplemented AST-to-AST
/// translation item is encountered.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
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

/// Allow or deny inferring details for the Rust project structure based on the
/// Python project structure.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum InferOption {
    /// Allows the transpiler to detect and rename an `__init__.py` file as
    /// `main.rs`.
    InitFileIntoMainRs,
    /// Allows the transpiler to detect and rename an `__init__.py` file as
    /// `lib.rs`.
    InitFileIntoLibRs,
    /// Allows the transpiler to detect and rename an `__init__` function as
    /// `main`.
    InitFnIntoMain,
}
