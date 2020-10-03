//! Usage:
//! ```
//! use serpent::{builder::TranspileModuleBuilder, config::TranspileConfig};
//! use std::path;
//!
//! fn main() -> Result<(), ApiError> {
//!     let mod_path = path::Path::new("path/to/module");
//!     let transpiled = TranspileModuleBuilder::new(&mod_path).config(&TranspileConfig::default()).transpile()?;
//!
//!     Ok(())
//! }
//! ```
use crate::{config::TranspileConfig, TranspiledString};
use crate::{transpile, TranspiledModule};

use fs_err as fs;
use std::{ops, path};

pub struct TranspileFileBuilder {
    inner: TranspileBuilder,
    py_file: path::PathBuf,
}

impl TranspileFileBuilder {
    pub fn new(py_file: impl AsRef<path::Path>) -> TranspileFileBuilder {
        TranspileFileBuilder {
            inner: TranspileBuilder::new(),
            py_file: py_file.as_ref().to_path_buf(),
        }
    }
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
    mod_path: path::PathBuf,
}

impl TranspileModuleBuilder {
    pub fn new(mod_path: impl AsRef<path::Path>) -> TranspileModuleBuilder where {
        TranspileModuleBuilder {
            inner: TranspileBuilder::new(),
            mod_path: mod_path.as_ref().to_path_buf(),
        }
    }
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

pub trait Transpile {
    type TranspileOutput;

    fn transpile(&self) -> Result<Self::TranspileOutput, crate::ApiError>;
}

impl Transpile for TranspileStringBuilder {
    type TranspileOutput = TranspiledString;

    fn transpile(&self) -> Result<Self::TranspileOutput, crate::ApiError> {
        let cfg = self.cfg.clone().unwrap_or_default();

        transpile::transpile_str(&self.py_source, &cfg)
    }
}

impl Transpile for TranspileFileBuilder {
    type TranspileOutput = TranspiledString;

    fn transpile(&self) -> Result<Self::TranspileOutput, crate::ApiError> {
        let cfg = self.cfg.clone().unwrap_or_default();

        let content = fs::read_to_string(&self.py_file)?;

        transpile::transpile_str(&content, &cfg)
    }
}

impl Transpile for TranspileModuleBuilder {
    type TranspileOutput = TranspiledModule;

    fn transpile(&self) -> Result<Self::TranspileOutput, crate::ApiError> {
        let cfg = self.cfg.clone().unwrap_or_default();

        transpile::transpile_module_dir(&self.mod_path, &cfg)
    }
}

macro_rules! impl_builder {
    ($builder:path) => {
        impl $builder {
            /// Uses `cfg` as the configuration for this builder.
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
