use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use tracing::debug;

use crate::{
    backend::codegen::Registers,
    ir::{
        block::NodeIndex,
        graph::{BlockIndex, IRGraph, END_BLOCK},
        node::Node,
    },
};
pub trait Register {
    fn as_assembly(&self) -> String;
    fn as_32_bit_assembly(&self) -> String;
    fn as_16_bit_assembly(&self) -> String;
    fn as_8_bit_assembly(&self) -> String;
    fn hardware_register(&self) -> bool;
    fn box_clone(&self) -> Box<dyn Register>;
}

#[derive(Debug, Clone)]
pub enum HardwareRegister {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl HardwareRegister {
    pub fn as_string(&self) -> String {
        match self {
            HardwareRegister::Rax => "rax".to_string(),
            HardwareRegister::Rbx => "rbx".to_string(),
            HardwareRegister::Rcx => "rcx".to_string(),
            HardwareRegister::Rdx => "rdx".to_string(),
            HardwareRegister::Rsi => "rsi".to_string(),
            HardwareRegister::Rdi => "rdi".to_string(),
            HardwareRegister::R8 => "r8".to_string(),
            HardwareRegister::R9 => "r9".to_string(),
            HardwareRegister::R10 => "r10".to_string(),
            HardwareRegister::R11 => "r11".to_string(),
            HardwareRegister::R12 => "r12".to_string(),
            HardwareRegister::R13 => "r13".to_string(),
            HardwareRegister::R14 => "r14".to_string(),
            HardwareRegister::R15 => "r15".to_string(),
        }
    }

    pub fn as_assembly_32_bit(&self) -> String {
        match self {
            HardwareRegister::Rax => "eax".to_string(),
            HardwareRegister::Rbx => "ebx".to_string(),
            HardwareRegister::Rcx => "ecx".to_string(),
            HardwareRegister::Rdx => "edx".to_string(),
            HardwareRegister::Rsi => "esi".to_string(),
            HardwareRegister::Rdi => "edi".to_string(),
            HardwareRegister::R8 => "r8d".to_string(),
            HardwareRegister::R9 => "r9d".to_string(),
            HardwareRegister::R10 => "r10d".to_string(),
            HardwareRegister::R11 => "r11d".to_string(),
            HardwareRegister::R12 => "r12d".to_string(),
            HardwareRegister::R13 => "r13d".to_string(),
            HardwareRegister::R14 => "r14d".to_string(),
            HardwareRegister::R15 => "r15d".to_string(),
        }
    }

    pub fn as_assembly_16_bit(&self) -> String {
        match self {
            HardwareRegister::Rax => "ax".to_string(),
            HardwareRegister::Rbx => "bx".to_string(),
            HardwareRegister::Rcx => "cx".to_string(),
            HardwareRegister::Rdx => "dx".to_string(),
            HardwareRegister::Rsi => "si".to_string(),
            HardwareRegister::Rdi => "di".to_string(),
            HardwareRegister::R8 => "r8w".to_string(),
            HardwareRegister::R9 => "r9w".to_string(),
            HardwareRegister::R10 => "r10w".to_string(),
            HardwareRegister::R11 => "r11w".to_string(),
            HardwareRegister::R12 => "r12w".to_string(),
            HardwareRegister::R13 => "r13w".to_string(),
            HardwareRegister::R14 => "r14w".to_string(),
            HardwareRegister::R15 => "r15w".to_string(),
        }
    }

    pub fn as_assembly_8_bit(&self) -> String {
        match self {
            HardwareRegister::Rax => "al".to_string(),
            HardwareRegister::Rbx => "bl".to_string(),
            HardwareRegister::Rcx => "cl".to_string(),
            _ => panic!(),
        }
    }
}

impl Register for HardwareRegister {
    fn as_assembly(&self) -> String {
        self.as_32_bit_assembly()
    }
    fn hardware_register(&self) -> bool {
        true
    }

    fn as_32_bit_assembly(&self) -> String {
        format!("%{}", self.as_assembly_32_bit())
    }

    fn as_16_bit_assembly(&self) -> String {
        format!("%{}", self.as_assembly_16_bit())
    }

    fn as_8_bit_assembly(&self) -> String {
        format!("%{}", self.as_assembly_8_bit())
    }

    fn box_clone(&self) -> Box<dyn Register> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct StackRegister {
    offset: usize,
}

impl StackRegister {
    pub fn new(offset: usize) -> StackRegister {
        StackRegister { offset }
    }

    pub fn as_assembly(&self) -> String {
        format!("{}(%rsp)", &self.offset)
    }
}

impl Register for StackRegister {
    fn as_assembly(&self) -> String {
        self.as_assembly()
    }

    fn as_32_bit_assembly(&self) -> String {
        self.as_assembly()
    }

    fn as_16_bit_assembly(&self) -> String {
        self.as_assembly()
    }

    fn as_8_bit_assembly(&self) -> String {
        self.as_assembly()
    }

    fn hardware_register(&self) -> bool {
        false
    }

    fn box_clone(&self) -> Box<dyn Register> {
        Box::new(self.clone())
    }
}

impl Display for StackRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(%rsp)", self.offset)
    }
}

pub struct RegisterAllocator {
    current_stack_offset: usize,
    registers: Registers,
    // Denotes that the key has been aliased by the value
    aliased_nodes: HashMap<(BlockIndex, NodeIndex), (BlockIndex, NodeIndex)>,
    available_hardware_register: Vec<HardwareRegister>,
}

impl RegisterAllocator {
    pub fn new() -> RegisterAllocator {
        RegisterAllocator {
            current_stack_offset: 0,
            registers: HashMap::new(),
            aliased_nodes: HashMap::new(),
            available_hardware_register: vec![
                //HardwareRegister::Rax,
                //HardwareRegister::Rbx,
                //HardwareRegister::Rcx,
                //HardwareRegister::Rdx,
                HardwareRegister::Rsi,
                HardwareRegister::Rdi,
                HardwareRegister::R8,
                HardwareRegister::R9,
                HardwareRegister::R10,
                HardwareRegister::R11,
                HardwareRegister::R12,
                HardwareRegister::R13,
                HardwareRegister::R14,
                HardwareRegister::R15,
            ],
        }
    }

    pub fn allocate_registers(mut self, graph: &IRGraph) -> (Registers, usize) {
        let mut visited = Vec::new();
        self.scan(END_BLOCK, graph, &mut visited);
        (self.registers, self.current_stack_offset)
    }

    pub fn scan(
        &mut self,
        block_index: BlockIndex,
        graph: &IRGraph,
        visited: &mut Vec<BlockIndex>,
    ) {
        let block = graph.get_block(block_index);
        for (predecessor, _) in block.entry_points() {
            if !visited.contains(predecessor) {
                visited.push(*predecessor);
                self.scan(*predecessor, graph, visited);
            }
        }
        debug!("Scanning block {}, for register allocation", block);
        for (node_index, node) in block.get_nodes().iter().enumerate() {
            if needs_register(node) {
                let register = self.get_available_register((block_index, node_index), graph);
                self.registers.insert((block_index, node_index), register);
            }
        }
    }

    pub fn get_available_register(
        &mut self,
        node: (BlockIndex, NodeIndex),
        ir_graph: &IRGraph,
    ) -> Box<dyn Register> {
        if self.aliased_nodes.contains_key(&node) {
            debug!("Not generating new register for node that is aliased by a following phi!");
        }
        debug!("Allocating new register for: {}", ir_graph.get_node(node));
        if self.has_available_hardware_register() {
            let register = self.available_hardware_register.pop().unwrap();
            let register: Box<dyn Register> = Box::new(register);
            register
        } else {
            let register = StackRegister::new(self.current_stack_offset);
            self.current_stack_offset += 8;
            let boxed_register: Box<dyn Register> = Box::new(register);
            boxed_register
        }
    }

    pub fn has_available_hardware_register(&self) -> bool {
        !self.available_hardware_register.is_empty()
    }

    pub fn current_stack_offset(&self) -> usize {
        self.current_stack_offset
    }
}

impl Clone for Box<dyn Register> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

fn needs_register(node: &Node) -> bool {
    !matches!(
        node,
        Node::Projection(_)
            | Node::Return(_)
            | Node::LowerEquals(_)
            | Node::Equals(_)
            | Node::ConditionalJump(_)
            | Node::Jump
    )
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for dyn Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Register: {}", self.as_assembly())
    }
}
