build:
    cargo build

test:
    cargo test

try:
    cargo run --example transpile-simple

alias run := try
