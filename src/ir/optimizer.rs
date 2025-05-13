use std::collections::HashMap;

use super::node::Node;

pub struct Optimizer {
    _known_nodes: HashMap<Node, Node>,
}

impl Optimizer {
    pub fn new() -> Optimizer {
        Optimizer {
            _known_nodes: HashMap::new(),
        }
    }

    pub fn transform(&self, node: Node) -> Node {
        node
    }
}
