//! Module with transpiler components. Allows transpiling Python source code to
//! Rust source code.
pub(crate) mod cursor;
pub(crate) mod identify_lines;
pub(crate) mod recontextualize;
pub(crate) mod remap_functions;
pub(crate) mod visit;

use std::result;

use super::*;
use crate::error::TranspileNodeError;
use cursor::Cursor;
use derive_deref::Deref;
use error::TranspileError;
use log::{debug, info, trace};
use num_bigint::BigInt;
use quote::ToTokens;
use recontextualize::recontextualize;
use remap_functions::remap_functions;
use rustpython_parser::ast;
use rustpython_parser::parser;
use syn;
use visit::*;

/// A type alias for `Result<T, serpent::SerpentError>`.
pub type Result<T> = result::Result<T, TranspileError>;

/// A buffer for messages concerning transpilation outcomes. Can be used to either inform the user
/// about what has been transpiled, or the transpiler about what to do in further processing steps.
/// This exists separately from logging and is meant to only report actionable items further down
/// the chain of transpilation.
type MessageBuffer = ();

pub struct TranspileOutput {
    pub out: String,
    pub messages: MessageBuffer,
}

/// Represents the concepts that are transpilable by serpent at API level, like Python module trees represented by [PyModule](struct.PyModule.html).
pub trait Transpile {
    fn transpile(&self) -> result::Result<TranspileOutput, SerpentError>;
}

/// Transpiles given Python source code to Rust source code.
pub fn transpile_python(src: PySource) -> Result<String> {
    // Create a Rust source-code generator by collecting information from the Python
    // source code

    let src = match src {
        PySource::Program(s, _) => s,
    };

    // Parse source into a program using RustPython
    info!("Parsing Python program...");
    let program = parser::parse_program(src)?;

    // FIXME: Recontextualization should be implemented at rustpython parser or lexer level such that comments are included
    // Add back unsupported context like comments
    info!("Recontextualizing Python program...");
    let py_nodes = recontextualize(src, program)?;

    let generator = RsGenerator {
        py_program: py_nodes,
    };

    // Generate the Rust source code with the information collected from the Python
    // source files
    info!("Generating Rust source code...");
    generator.generate()
}

/// Represents what's required to generate a Rust program from Python source.
///
/// Call .generate() to produce a program.
struct RsGenerator {
    /// The original Python program parsed as a vector of Python statements, containing
    /// single or multiline statements and unknown entries (whitespace,
    /// comments, other). Contains location as context for each entry.
    ///
    /// FIXME: Comments are ignored currently, and not included in this vector. They should be
    /// included here later, when rustpython parser can help with parsing the comments.
    py_program: Vec<PyNode>,
}

impl RsGenerator {
    /// Generates the Rust source code
    pub fn generate(&self) -> Result<String> {
        // Create a Rust node for each Python node
        let mut rs_nodes = self.translate_ast()?;

        // Go over the Rust AST and do function mappings
        RsGenerator::remap_functions(&mut rs_nodes);

        // Generate the Rust source code from the Rust AST nodes
        let rs_src = RsGenerator::codegen(&rs_nodes);

        Ok(rs_src)
    }

    /// Converts the Python AST nodes into Rust AST nodes.
    fn translate_ast(&self) -> Result<Vec<RsNode>> {
        let mut rs_nodes = vec![];

        let mut cursor = Cursor::new(&self.py_program);

        while let Some(node) = cursor.advance() {
            let node_no = cursor.idx();
            let src = &node.src;
            let node = &node.node;
            info!("Node {}: `{}`", node_no, src);
            let rs_node = match node {
                PyNodeKind::Statement(stmt) => {
                    match visit_statement(&stmt.node, Some(&stmt.location), Some(&cursor)) {
                        Ok(rs_stmt) => RsNode::Statement(rs_stmt),
                        Err(err) => {
                            return Err(TranspileError::TranspileNode {
                                line: src.to_string(),
                                line_no: node_no,
                                reason: err,
                            })
                        }
                    }
                }
                PyNodeKind::Newline(_located) => RsNode::Newline,
                PyNodeKind::Comment(ast::Located { node, .. }) => RsNode::Comment(node.clone()),
            };
            debug!("Node {} -> {:?}", node_no, &rs_node);
            rs_nodes.push(rs_node);
        }

        Ok(rs_nodes)
    }

    fn remap_functions(nodes: &mut Vec<RsNode>) {
        for node in nodes {
            // Pick out statements, no comments or newlines
            if let RsNode::Statement(stmt) = node {
                // Recurse through each statement, visiting all method calls, remapping the print statements
                remap_functions(stmt);
            };
        }
    }

    /// Generates Rust source code from Rust AST nodes.
    fn codegen(rs_nodes: &[RsNode]) -> String {
        // Format/print Rust statements
        let formatted = rs_nodes
            .iter()
            .enumerate()
            .map(|(node_no, node)| match node {
                RsNode::Statement(stmt) => {
                    let tokens = stmt.to_token_stream();

                    let relevant = tokens
                        .to_string()
                        .lines()
                        .map(|x| x.trim().to_owned())
                        .collect::<Vec<String>>();
                    info!("Node {} = Stmt::{:?} -> {:?}", node_no, stmt, &relevant);

                    relevant
                }
                RsNode::Newline => vec!["".to_owned()],
                RsNode::Comment(s) => vec![format!("// {}", s)],
            })
            .flatten();

        // Insert main
        let with_main = std::iter::once("fn main() {".to_owned())
            .chain(formatted)
            .chain(std::iter::once("}".to_owned()));

        // Catenate statements with newlines and return
        let out = with_main.fold(String::new(), |acc, next| acc + &next + "\n");

        // Format again
        let (_, file_map, _) = rustfmt::format_input::<Vec<u8>>(
            rustfmt::Input::Text(out),
            &rustfmt::config::Config::default(),
            None,
        )
        .unwrap();

        let output = &file_map.first().unwrap().1;
        let output_str = output.chars().map(|(c, _)| c).collect::<String>();
        output_str
    }
}

/// A statement, newline or comment of Python with associated context.
/// Everything that's required to create an expression in Rust.
#[derive(Debug)]
pub(crate) struct PyNode {
    src: String,
    node: PyNodeKind,
}

#[derive(Debug)]
pub(crate) enum PyNodeKind {
    Statement(ast::Located<ast::StatementType>),
    Newline(ast::Located<()>),
    Comment(ast::Located<String>),
}

impl PyNode {
    pub(crate) fn new(src: String, node: PyNodeKind) -> PyNode {
        PyNode { src, node }
    }
}

impl cursor::Node for PyNode {}

/// Convert a Python AST statement into a `PyNodeKind::Statement`.
impl From<ast::Located<ast::StatementType>> for PyNodeKind {
    fn from(stmt: ast::Located<ast::StatementType>) -> Self {
        PyNodeKind::Statement(stmt)
    }
}

#[derive(Debug)]
enum RsNode {
    Statement(syn::Stmt),
    Newline,
    Comment(String),
}

#[derive(Clone, Deref)]
struct RsStmt(pub syn::Stmt);
