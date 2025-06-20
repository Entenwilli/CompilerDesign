use std::{collections::HashMap, fmt::Display};

use super::{graph::BlockIndex, node::Node};

pub type NodeIndex = usize;

#[derive(Debug)]
pub struct Block {
    // Denote that block can be entered from block (key) at node index (value) within that block
    entry_points: HashMap<BlockIndex, Vec<NodeIndex>>,
    phis: Vec<NodeIndex>,
    nodes: Vec<Node>,
    name: String,
}

impl Block {
    pub fn new(name: String) -> Block {
        Block {
            entry_points: HashMap::new(),
            phis: Vec::new(),
            nodes: Vec::new(),
            name,
        }
    }

    pub fn register_entry_point(&mut self, block: BlockIndex, node_index: NodeIndex) {
        if self.entry_points.contains_key(&block) {
            let mut value = self.entry_points.remove(&block).unwrap();
            value.push(node_index);
            self.entry_points.insert(block, value);
        } else {
            self.entry_points.insert(block, vec![node_index]);
        }
    }

    pub fn entry_points(&self) -> &HashMap<BlockIndex, Vec<NodeIndex>> {
        &self.entry_points
    }

    pub fn register_node(&mut self, node: Node) -> NodeIndex {
        if let Node::Phi(_) = node {
            self.phis.push(self.nodes.len());
        }
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

    pub fn phis(&self) -> &Vec<NodeIndex> {
        &self.phis
    }

    pub fn get_last_node_index(&self) -> NodeIndex {
        self.nodes.len() - 1
    }

    pub fn get_label(&self, index: usize) -> String {
        let mut label = self.name.to_owned();
        label.push_str(&index.to_string());
        label
    }

    pub fn empty(&self) -> bool {
        self.nodes.is_empty()
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
