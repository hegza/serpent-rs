use serpent::{Transpile, TranspileConfig, TranspileStringBuilder};
use test_case::test_case;

#[test_case(
"a = 5"
=>
"fn main() {
    let a = 5;
}"
;
"simple local transpiles right"
)]
#[test_case(
    include_str!("input/simple_multiline.py") =>
    include_str!("input/simple_multiline.rs");
    "simple multiline transpiles right")]
fn transpile_runnable(stmt: &str) -> String {
    TranspileStringBuilder::new(stmt.to_owned())
        .config(TranspileConfig {
            infer_main: true,
            ..Default::default()
        })
        .transpile()
        .unwrap()
        .into_string()
}
