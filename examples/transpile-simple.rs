use serpent::{transpile, ProgramKind, PySource};
use sourcefile::SourceFile;

fn main() {
    let source_file = {
        let filename = "examples/input/simple.py";
        let sf = SourceFile::default();
        sf.add_file(filename).unwrap()
    };
    let source = source_file.contents;

    println!("Source:\n{}", &source);
    let result = transpile(PySource::Program(&source, ProgramKind::Runnable));
    println!("Result:\n{}", &result.unwrap());
}
