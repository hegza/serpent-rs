use crate::{builder::Transpile, TranspileStringBuilder};

#[test]
fn trivial_builder_returns_ok() {
    let out = TranspileStringBuilder::new("a = 0".to_owned()).transpile();

    assert!(out.is_ok());
}
