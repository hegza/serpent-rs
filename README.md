## Build Notes
- Runs on stable Rust.
- Use `just run` to test the current use case.
- Format code with: `cargo +nightly fmt`

## Developer Notes
- Comments are currently included with a workaround. I think they should be parsed into the AST at rustpython parser level.
- Remember to open vs-code project in Windows, not  in Windows Subsystem for Linux, because rust-analyzer tends to not work right on WSL.
- Fidelity print where AST does not change and pretty print where it does.