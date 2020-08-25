use serpent::transpile_file;
use sourcefile::SourceFile;

use std::fmt;

/// (source code string, language, line numbers?)
#[derive(Debug, Clone)]
struct SourceView<'s>(&'s str, Language, bool);

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

fn main() {
    pretty_env_logger::init();

    let filename = "examples/py/simple.py";
    let source_file = {
        let sf = SourceFile::default();
        sf.add_file(filename).unwrap()
    };
    let source = SourceView(&source_file.contents, Language::Python, true);

    println!("Source:\n{}", &source);
    let result = transpile_file(filename).unwrap();
    println!();
    let view = SourceView(&result, Language::Rust, false);
    println!("Result:\n{}", &view);
}
