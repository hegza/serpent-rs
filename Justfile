build:
    cargo build

test:
    cargo test

try:
    cargo run --example transpile_simple

alias run := try
