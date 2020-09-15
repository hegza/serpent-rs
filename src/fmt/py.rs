use std::fmt;

use py::Located;
use rustpython_parser::ast as py;

#[derive(PartialEq)]
pub struct InvisibleLocation<T>(Located<T>);

impl<T> fmt::Debug for InvisibleLocation<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.node)
    }
}

impl<T> From<Located<T>> for InvisibleLocation<T> {
    fn from(l: Located<T>) -> Self {
        Self(l)
    }
}

impl<T> std::ops::Deref for InvisibleLocation<T> {
    type Target = Located<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
