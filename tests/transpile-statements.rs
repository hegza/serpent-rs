use serpent::transpile;

#[test]
fn transpile_local() {
    env_logger::init();

    let stmt = "a = 5";

    let transpiled = transpile(stmt);

    assert_eq!(transpiled, "let a = 5 ;");
}