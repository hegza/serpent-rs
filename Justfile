example := "transpile_simple"

build:
    cargo build

test:
    cargo test

try:
    cargo run --example {{example}}

backtrace:
    RUST_BACKTRACE=1 cargo +nightly run --example {{example}}

alias run := try
