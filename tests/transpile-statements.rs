use serpent::{transpile, ProgramKind, PySource};

#[test]
fn transpile_local() {
    let stmt = "a = 5";

    let transpiled = transpile(PySource::Program(stmt, ProgramKind::Runnable)).unwrap();

    assert_eq!(transpiled, "fn main() {\n    let a = 5;\n}\n");
}

#[test]
fn transpile_multiline() {
    let py = include_str!("input/simple_multiline.py");

    let transpiled = transpile(PySource::Program(py, ProgramKind::Runnable)).unwrap();

    assert_eq!(transpiled, include_str!("input/simple_multiline.rs"));
}
