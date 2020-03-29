use serpent::transpile;

#[test]
fn transpile_local() {
    let stmt = "a = 5";

    let transpiled = transpile(stmt);

    assert_eq!(transpiled, "let a = 5;\n");
}

#[test]
fn transpile_multiline() {
    let py = include_str!("input/simple_multiline.py");

    let transpiled = transpile(py);

    assert_eq!(transpiled, include_str!("input/simple_multiline.rs"));
}
