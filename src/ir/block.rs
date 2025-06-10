use std::{collections::HashMap, fmt::Display};

use super::{graph::BlockIndex, node::Node};

pub type NodeIndex = usize;

pub struct Block {
    // Denote that block can be entered from block (key) at node index (value) within that block
    entry_points: HashMap<BlockIndex, NodeIndex>,
    nodes: Vec<Node>,
    name: String,
}

impl Block {
    pub fn new(name: String) -> Block {
        Block {
            entry_points: HashMap::new(),
            nodes: Vec::new(),
            name,
        }
    }

    pub fn register_entry_point(&mut self, block: BlockIndex, node_index: NodeIndex) {
        self.entry_points.insert(block, node_index);
    }

    pub fn entry_points(&self) -> &HashMap<BlockIndex, NodeIndex> {
        &self.entry_points
    }

    pub fn register_node(&mut self, node: Node) -> NodeIndex {
        self.nodes.push(node);
        return self.nodes.len() - 1;
    }

    pub fn get_node(&self, node_index: NodeIndex) -> &Node {
        self.nodes.get(node_index).expect("Expected node at index")
    }

    pub fn get_node_mut(&mut self, node_index: NodeIndex) -> &mut Node {
        self.nodes
            .get_mut(node_index)
            .expect("Expected node at index")
    }

    pub fn get_nodes(&self) -> &Vec<Node> {
        &self.nodes
    }

    pub fn get_last_node_index(&self) -> NodeIndex {
        self.nodes.len() - 1
    }

    pub fn get_label(&self, index: usize) -> String {
        let mut label = self.name.to_owned();
        label.push_str(&index.to_string());
        label
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "-- Block {} --", self.name)?;
        writeln!(f, "Entry Points: {:?}", self.entry_points)?;
        for node in &self.nodes {
            writeln!(f, "{}", node)?;
        }
        Ok(())
    }
}
