use serpent::transpile;
use sourcefile::SourceFile;

fn main() {
    let source_file = {
        let filename = "examples/input/simple.py";
        let sf = SourceFile::default();
        sf.add_file(filename).unwrap()
    };
    let source = source_file.contents;

    println!("Source:\n{}", &source);
    let result = transpile(&source);
    println!("Result:\n{}", &result);
}
