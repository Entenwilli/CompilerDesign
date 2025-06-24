use std::fmt::Display;

use super::{
    block::{Block, NodeIndex},
    node::Node,
};

pub type BlockIndex = usize;

pub const START_BLOCK: usize = 0;
pub const END_BLOCK: usize = 1;

pub struct IRGraph {
    name: String,
    blocks: Vec<Block>,
}

impl IRGraph {
    pub fn new(name: String) -> IRGraph {
        IRGraph {
            name,
            blocks: vec![
                Block::new("start".to_string()),
                Block::new("end".to_string()),
            ],
        }
    }

    pub fn start_block(&self) -> &Block {
        self.blocks.get(START_BLOCK).expect("Start Block missing!")
    }

    pub fn start_block_mut(&mut self) -> &mut Block {
        self.blocks
            .get_mut(START_BLOCK)
            .expect("Start Block missing!")
    }

    pub fn register_block(&mut self, block: Block) -> BlockIndex {
        self.blocks.push(block);
        self.blocks.len() - 1
    }

    pub fn get_block(&self, block_index: BlockIndex) -> &Block {
        self.blocks
            .get(block_index)
            .expect("Expected block at block index")
    }

    pub fn get_node(&self, data: (BlockIndex, NodeIndex)) -> &Node {
        self.blocks
            .get(data.0)
            .expect("Expected block at block index")
            .get_node(data.1)
    }

    pub fn get_block_mut(&mut self, block_index: BlockIndex) -> &mut Block {
        self.blocks
            .get_mut(block_index)
            .expect("Expected block at block index")
    }

    pub fn get_blocks(&self) -> &Vec<Block> {
        &self.blocks
    }

    pub fn end_block(&self) -> &Block {
        self.blocks.get(END_BLOCK).expect("End Block missing!")
    }

    pub fn end_block_mut(&mut self) -> &mut Block {
        self.blocks.get_mut(END_BLOCK).expect("End Block missing!")
    }

    pub fn name(&self) -> &String {
        &self.name
    }
}

impl Default for IRGraph {
    fn default() -> Self {
        Self::new("default".to_string())
    }
}

impl Display for IRGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, block) in self.blocks.iter().enumerate() {
            writeln!(f, "{} -- {}", index, block)?;
        }
        Ok(())
    }
}
