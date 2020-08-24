use anyhow::{Context, Result};

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

    let source_module = serpent::import_module(DIR);
    let transpiled = source_module?
        .transpile()
        .context(format!("unable to transpile module from path: \"{}\"", DIR))?;
    let program = SourceView(&transpiled.program, Language::Rust, true);
    println!("Transpiled Rust source code:\n```rust\n{}\n```", program);

    Ok(())
}
