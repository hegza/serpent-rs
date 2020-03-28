build:
    cargo build

test:
    cargo run --example transpile-simple

test-minimal:
    cargo run --example transpile-local

alias run := test
