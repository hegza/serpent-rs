pub mod py;
mod rs;

use std::fmt;

/// A custom `Debug` trait for AST nodes. Allows us to print AST nodes for
/// user-facing applications eg. a CLI.
pub trait AstString: fmt::Debug {
    fn to_ast_string(&self) -> String {
        // Use debug print by default
        format!("{:?}", self)
    }
}

/// A custom `Debug` trait for non-recursing print.
pub trait OpaqueDebug {
    /// Formats self such that no recursion happens.
    fn opaque_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

pub struct OpaqueFmt<F>(pub F)
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result;

impl<F> fmt::Debug for OpaqueFmt<F>
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.0)(f)
    }
}

/// Opaque display passes through vectors rendering them vertically, calling the
/// inner implementation.
impl<T> OpaqueDebug for Vec<T>
where
    T: OpaqueDebug,
{
    fn opaque_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;

        // Call opaque-format for each child
        let mut iter = self.iter().peekable();
        while let Some(elem) = iter.next() {
            write!(f, "\t")?;
            elem.opaque_fmt(f)?;

            if iter.peek().is_some() {
                writeln!(f, ",")?;
            }
        }

        writeln!(f, "\n}}")
    }
}
