/// Wraps the AST iteration such that a deeply nested `visit` function can view it's context for eg.
/// debugging.
pub struct Cursor<'n, N>
where
    N: Node,
{
    nodes: &'n [N],
    next: usize,
}

/// Represents something that can be iterated over with a Cursor.
pub trait Node {}

impl<'n, N> Cursor<'n, N>
where
    N: Node,
{
    pub fn new(nodes: &[N]) -> Cursor<N> {
        Cursor { nodes, next: 0 }
    }

    // Yields the node on the cursor, and moves the cursor
    pub fn advance<'a>(&'a mut self) -> Option<&'n N> {
        if self.next < self.nodes.len() {
            self.next += 1;
            return Some(self.cur_node());
        }
        None
    }

    pub fn idx(&self) -> usize {
        self.next - 1
    }

    // Returns the current node
    pub fn cur_node(&self) -> &'n N {
        &self.nodes[self.next - 1]
    }
}
