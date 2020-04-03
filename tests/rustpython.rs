use rustpython_parser::parser::parse_statement;

#[test]
fn invalid_statements() {
    // Missing rhs
    assert!(parse_statement("a =").is_err());

    // Partial multiline
    assert!(parse_statement("add(").is_err());
}

#[test]
fn valid_statements() {
    assert!(parse_statement("a = 0").is_ok());

    // Multiline
    assert!(parse_statement(
        "\
        add(
        a + b)"
    )
    .is_ok());
}
