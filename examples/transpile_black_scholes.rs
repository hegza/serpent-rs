use anyhow::{Context, Result};

use serpent::transpile_module;
use std::fmt;

const DIR: &str = "examples/py/black_scholes/";
/// (source code string, language, line numbers?)
#[derive(Debug, Clone)]
pub struct SourceView<'s>(&'s str, Language, bool);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
enum Language {
    Python,
    Rust,
}

impl<'s> fmt::Display for SourceView<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lang = match self.1 {
            Language::Python => "py",
            Language::Rust => "rs",
        };

        let show_line_numbers = self.2;
        if !show_line_numbers {
            write!(f, "```{}\n{}\n```)", lang, &self.0)
        } else {
            write!(f, "```{}\n", lang)?;
            for (line_no, line) in self.0.lines().enumerate() {
                write!(f, "{:>3} {}\n", line_no + 1, line)?;
            }
            write!(f, "```",)
        }
    }
}

fn main() -> Result<()> {
    pretty_env_logger::init();

    let transpiled = transpile_module(DIR)
        .context(format!("unable to transpile module from path: \"{}\"", DIR))?;
    for serpent::output::TranspiledFile {
        source_path: path,
        kind: _kind,
        content: transpiled,
    } in transpiled.files
    {
        let program = SourceView(&transpiled.as_str(), Language::Rust, true);
        println!("Transpiled Rust source code for {:?}:\n{}", path, program);
    }

    Ok(())
}
