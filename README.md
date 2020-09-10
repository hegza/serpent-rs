## Build Notes
- Runs on stable Rust.
- Use `just run` to test the current use case.
- Format code with: `cargo +nightly fmt`

## Developer Notes
- Comments are currently included with a workaround. They are hard to parse at AST level, because comments can be anywhere. Perhaps one day there can be character by character transpile tracing that can place the comments exactly where they belong.
- Fidelity print where AST does not change and pretty print where it does.
- The general convention is that Python AST nodes from RustPython are used as `py::*` and Rust AST nodes from rustc are used as `rs::*`, while crate local higher-level wrapping types are used as `python::*` and `rust::*` respectively.
