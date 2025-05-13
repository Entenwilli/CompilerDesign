use std::{collections::HashMap, fmt::Display};

use super::node::{BlockData, Node};

pub const START_BLOCK: usize = 0;
pub const END_BLOCK: usize = 1;

pub struct IRGraph {
    nodes: HashMap<usize, Node>,
    next_node_index: usize,
}

impl IRGraph {
    pub fn new() -> IRGraph {
        IRGraph {
            nodes: HashMap::from([
                (START_BLOCK, Node::Block(BlockData::new(START_BLOCK))),
                (END_BLOCK, Node::Block(BlockData::new(END_BLOCK))),
            ]),
            next_node_index: 2,
        }
    }

    pub fn register_node(&mut self, node: Node) -> usize {
        self.nodes.insert(self.next_node_index, node);
        self.next_node_index += 1;
        self.next_node_index - 1
    }

    pub fn remove_node(&mut self, index: usize) -> Node {
        self.nodes
            .remove(&index)
            .expect("Cannot remove node at index")
    }

    pub fn get_node(&self, index: usize) -> &Node {
        self.nodes.get(&index).expect("Cannot find node at index")
    }

    pub fn get_node_mut(&mut self, index: usize) -> &Node {
        self.nodes
            .get_mut(&index)
            .expect("Cannot find node at index")
    }

    pub fn get_predecessors(&self, index: usize) -> &Vec<usize> {
        let node = self
            .nodes
            .get(&index)
            .expect("Cannot find node for predecessors");
        node.predecessors()
    }

    pub fn start_block(&self) -> &Node {
        self.nodes.get(&START_BLOCK).expect("Start Block missing!")
    }

    pub fn end_block(&self) -> &Node {
        self.nodes.get(&END_BLOCK).expect("End Block missing!")
    }

    pub fn end_block_mut(&mut self) -> &mut Node {
        self.nodes.get_mut(&END_BLOCK).expect("End Block missing!")
    }
}

impl Default for IRGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for IRGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "START: {}\n", self.get_node(START_BLOCK))?;
        for i in END_BLOCK + 1..self.next_node_index {
            write!(f, "{}: {}\n", i, self.get_node(i))?
        }
        write!(f, "END: {}\n", self.get_node(END_BLOCK))?;
        Ok(())
    }
}
