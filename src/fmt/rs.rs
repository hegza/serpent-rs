use std::fmt::Debug;

use crate::transpile::rust;
use crate::{fmt::AstString, transpile::ast_to_ast::dummy};
use itertools::Itertools;
use rustc_ap_rustc_ast::{ast, visit::Visitor};

impl<'a> AstString for &Vec<rust::NodeKind> {
    fn to_ast_string(&self) -> String {
        let elems = self.iter().map(|elem| elem.to_ast_string()).join(", ");
        format!("[{}]", elems)
    }
}

impl AstString for rust::NodeKind {
    fn to_ast_string(&self) -> String {
        /*
        let mut generator = AstStringGenerator::default();
        match self {
            rust::NodeKind::ExtendedItem(item) => generator.visit_item(item),
            rust::NodeKind::Item(item) => unimplemented!(),
            rust::NodeKind::ExtendedStmt(stmt) => generator.visit_stmt(stmt),
            rust::NodeKind::Stmt(stmt) => generator.visit_stmt(&dummy::stmt(stmt.clone())),
            rust::NodeKind::Newline => return format!("Newline"),
            rust::NodeKind::Comment(content) => {
                return format!("Comment {{ {:?} }}", content.to_string())
            }
        }
        generator.generate()*/
        format!("{:?}", self)
    }
}

/*
#[derive(Default, Debug)]
struct AstStringGenerator(String);

impl AstStringGenerator {
    fn generate(self) -> String {
        self.0
    }
}

impl<'ast> Visitor<'ast> for AstStringGenerator {
    fn visit_stmt(&mut self, s: &'ast ast::Stmt) {
        self.0.push_str(&format!("{:?}", s));
        rustc_ap_rustc_ast::visit::walk_stmt(self, s)
    }
}
*/
