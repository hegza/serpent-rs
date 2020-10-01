use crate::transpile;
use crate::{config::TranspileConfig, TranspiledString};

use std::ops;

pub struct TranspileFileBuilder {
    inner: TranspileBuilder,
}

pub struct TranspileStringBuilder {
    inner: TranspileBuilder,
    py_source: String,
}

impl TranspileStringBuilder {
    pub fn new(py_source: String) -> TranspileStringBuilder {
        TranspileStringBuilder {
            inner: TranspileBuilder::new(),
            py_source,
        }
    }
}

pub struct TranspileModuleBuilder {
    inner: TranspileBuilder,
}

/// Properties common to all transpilation procedures. Transpile*Builders deref
/// into Transpile Builder.
pub struct TranspileBuilder {
    cfg: Option<TranspileConfig>,
}

impl TranspileBuilder {
    /// This not public because `TranspileBuilder` cannot be created directly.
    /// Use `Transpile*Builder` instead.
    fn new() -> TranspileBuilder {
        TranspileBuilder { cfg: None }
    }
}

impl Transpile for TranspileStringBuilder {
    type TranspileOutput = TranspiledString;

    fn transpile(&self) -> Result<Self::TranspileOutput, crate::ApiError> {
        let cfg = self.cfg.clone().unwrap_or_default();

        transpile::transpile_str(&self.py_source, &cfg)
    }
}

pub trait Transpile {
    type TranspileOutput;

    fn transpile(&self) -> Result<Self::TranspileOutput, crate::ApiError>;
}

macro_rules! impl_builder {
    ($builder:path) => {
        impl $builder {
            pub fn config(mut self, cfg: TranspileConfig) -> Self {
                self.inner.cfg = Some(cfg);
                self
            }
        }
    };
}

impl_builder!(TranspileModuleBuilder);
impl_builder!(TranspileFileBuilder);
impl_builder!(TranspileStringBuilder);

macro_rules! impl_deref {
    ($src:path) => {
        impl ops::Deref for $src {
            type Target = TranspileBuilder;

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
}

impl_deref!(TranspileModuleBuilder);
impl_deref!(TranspileFileBuilder);
impl_deref!(TranspileStringBuilder);
