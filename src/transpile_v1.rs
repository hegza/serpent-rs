use std::{iter::Map, result, slice::Iter};

use crate::{
    error::TranspileNodeError,
    transpile_v0::{cursor::Cursor, visit::visit_statement},
};
use proc_macro2::TokenStream;
use quote::ToTokens;
use rustpython_parser::{
    ast::{Program, Statement, StatementType},
    error::ParseError,
    location::Location,
    parser,
};
use syn::parse::Parse;
use thiserror::Error as ThisError;

struct TranspileOutput {
    src: String,
}

/// An error that occurred while transpiling Python into Rust.
#[derive(ThisError, Debug)]
enum TranspileError {
    /// A parsing error that occurred while parsing a string into a Python AST
    /// with RustPython.
    #[error("Python parse error")]
    Parse(#[from] ParseError),
    /// An error that occurred while transpiling the Python AST into Rust. This
    /// AST node could not be transpiled.
    #[error("Transpile error`")]
    TranspileNode {
        #[from]
        reason: TranspileNodeError,
    },
}

/// A type alias for `Result<T, TranspileError>`.
type Result<T> = result::Result<T, TranspileError>;

#[derive(Debug)]
struct PyAstNode {
    /// Index of first character of the token stream in the original source
    start_idx: usize,
    /// Index of last character of the token stream in the original source
    end_idx: usize,
    /// Tokens of the node in the original source
    tokens: String,
    /// Python AST node
    stmt: StatementType,
}

struct RsAstNode {
    tokens: TokenStream,
    stmt: syn::Stmt,
}

/// Converts the Python contents of the string into Rust.
fn transpile(src: &str) -> Result<TranspileOutput> {
    // Parse Python source into an AST
    let python_ast = parser::parse_program(src)?;

    // Complete span in original source for each AST node, because rustpython parser does not record end of span
    let spans = complete_span(&python_ast.statements, src);

    // Marshal `PyAstNode` for text-processing
    let python_ast = python_ast
        .statements
        .into_iter()
        .zip(&spans)
        .map(|(stmt, span)| PyAstNode {
            start_idx: span.0,
            end_idx: span.1,
            tokens: src[span.0..span.1].to_owned(),
            stmt: stmt.node,
        })
        .collect::<Vec<PyAstNode>>();

    // Create a replacement mapping by visiting each AST node
    let replacement_map = python_ast.iter().map(|PyAstNode { stmt, .. }| {
        let stmt = visit_statement(&stmt, None, None).unwrap();
        let tokens = stmt.to_token_stream();
        RsAstNode { tokens, stmt }
    });

    // Write the Rust source
    let rust_source = write_rust(&python_ast, replacement_map, src)?;

    Ok(TranspileOutput { src: rust_source })
}

fn complete_span(stmts: &[Statement], src: &str) -> Vec<(usize, usize)> {
    // Exceptions for cases where windows() cannot be created
    if stmts.len() == 0 {
        return vec![];
    } else if stmts.len() == 1 {
        let start = 0;
        let end = src.len() - 1;
        return vec![(start, end)];
    }

    let mut src_it = src.chars();
    let mut src_cur_row = 1;
    let mut src_cur_col = 1;
    let mut src_cur_idx = 0;

    let mut spans: Vec<(usize, usize)> = stmts
        .windows(2)
        .map(|window| {
            let (cur, next) = (&window[0], &window[1]);
            let stmt_row = cur.location.row();
            let stmt_col = cur.location.column();

            // Skip chars in src until start of statement
            src_it.find(|src_char| {
                if src_cur_row == stmt_row && src_cur_col == stmt_col {
                    return true;
                }

                // Add one col or row, depending on char, and add one index
                if src_char == &'\n' {
                    src_cur_row += 1;
                    src_cur_col = 0;
                } else {
                    src_cur_col += 1;
                }
                src_cur_idx += 1;

                false
            });

            let start_idx = src_cur_idx;

            // FIXME: find the end of `cur` instead of using start of `next`
            let stmt_row = next.location.row();
            let stmt_col = next.location.column();

            // Skip chars in src until start of next statement
            src_it.find(|src_char| {
                if src_cur_row == stmt_row && src_cur_col == stmt_col {
                    return true;
                }

                // Add one col or row, depending on char, and add one index
                if src_char == &'\n' {
                    src_cur_row += 1;
                    src_cur_col = 0;
                } else {
                    src_cur_col += 1;
                }
                src_cur_idx += 1;

                false
            });

            // The end character is the one before the start of the next one
            let end_idx = src_cur_idx;

            (start_idx, end_idx)
        })
        .collect();

    // Exception for the last element
    let start = src_cur_idx;
    let end = src.len();
    spans.push((start, end));

    spans
}

fn write_rust<'r, F>(
    python_ast: &[PyAstNode],
    replacement_map: Map<Iter<'r, PyAstNode>, F>,
    src: &str,
) -> Result<String>
where
    F: (FnMut(&'r PyAstNode) -> RsAstNode),
{
    // Create a new source file for Rust.
    let mut rust_source = String::new();
    let mut read_idx = 0;

    // Visit each mapped element (comment, whitespace, statement) by enumeration, replacing it with the created Rust version
    for (replaceable_node, replacement) in python_ast.iter().zip(replacement_map) {
        // Copy from source as-is until replaceable node
        let copy_source = &src[read_idx..replaceable_node.start_idx];
        rust_source.push_str(copy_source);

        // Advance cursor to match what was copied from original source
        let copy_len = replaceable_node.start_idx - read_idx;
        read_idx += copy_len;

        // Copy transpiled node from replacement map
        let transpiled = replacement.tokens.to_string();
        rust_source.push_str(&transpiled);
        let original_len = replaceable_node.end_idx - replaceable_node.start_idx;
        read_idx += original_len;
    }

    Ok(rust_source)
}

#[cfg(test)]
mod test {
    use super::*;

    const SRC_1: &str = "\
def a():

    b = 1
";
    const SPANS_1: &[(usize, usize); 1] = &[(0, 19)];
    const RUST_1: &str = "\
pub fn a ( ) { let b = 1 ; }";

    const SRC_2: &str = "\
def a():

    b = 1

c = 2
";
    const SPANS_2: &[(usize, usize); 2] = &[(0, 21), (21, 27)];
    const RUST_2: &str = "\
pub fn a() { let b = 1; }

let c = 2;
";

    #[test]
    fn span_completes() {
        // SRC_1
        let ast = parser::parse_program(SRC_1).unwrap();
        let spans = complete_span(&ast.statements, SRC_1);

        assert_eq!(&spans, SPANS_1);

        // SRC_2
        let ast = parser::parse_program(SRC_2).unwrap();
        let spans = complete_span(&ast.statements, SRC_2);

        for (span, correct) in spans.iter().zip(SPANS_2) {
            let chars = &SRC_2[span.0..span.1];
            let correct_chars = &SRC_2[correct.0..correct.1];
            assert_eq!(
                span, correct,
                "\n{:?}\n!=\n{:?}\nin:\n{:?}\n",
                chars, correct_chars, SRC_2
            );
        }
    }

    #[test]
    fn transpiles() {
        let out = transpile(SRC_1).unwrap();
        assert_eq!(out.src, RUST_1);

        let out = transpile(SRC_2).unwrap();
        assert_eq!(out.src, RUST_2);
    }
}
