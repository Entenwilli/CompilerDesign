use std::{collections::HashMap, fmt::Display};

use crate::ir::{
    graph::{IRGraph, END_BLOCK},
    node::Node,
};

pub trait Register {
    fn as_assembly(&self) -> String;
}

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
            HardwareRegister::Rsi => "rs".to_string(),
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
}

impl Register for HardwareRegister {
    fn as_assembly(&self) -> String {
        format!("%{}", self.as_string())
    }
}

pub struct StackRegister {
    offset: usize,
}

impl StackRegister {
    pub fn new(offset: usize) -> StackRegister {
        StackRegister { offset }
    }

    pub fn as_assembly(&self) -> String {
        format!("rip[{}]", &self.offset)
    }
}

impl Register for StackRegister {
    fn as_assembly(&self) -> String {
        self.as_assembly()
    }
}

impl Display for StackRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[rip, #{}]", self.offset)
    }
}

pub struct RegisterAllocator<'a> {
    current_stack_offset: usize,
    registers: HashMap<&'a Node, Box<dyn Register>>,
    available_hardware_register: Vec<HardwareRegister>,
}

impl<'a> RegisterAllocator<'a> {
    pub fn new() -> RegisterAllocator<'a> {
        RegisterAllocator {
            current_stack_offset: 0,
            registers: HashMap::new(),
            available_hardware_register: vec![
                //HardwareRegister::Rax,
                HardwareRegister::Rbx,
                HardwareRegister::Rcx,
                HardwareRegister::Rdx,
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

    pub fn allocate_registers(mut self, graph: &'a IRGraph) -> HashMap<&Node, Box<dyn Register>> {
        let mut visited = Vec::new();
        visited.push(END_BLOCK);
        self.scan(END_BLOCK, graph, &mut visited);
        self.registers
    }

    pub fn scan(&mut self, current_index: usize, graph: &'a IRGraph, visited: &mut Vec<usize>) {
        let node = graph.get_node(current_index);
        for predecessor in node.predecessors() {
            if !visited.contains(predecessor) {
                visited.push(*predecessor);
                self.scan(*predecessor, graph, visited);
            }
        }
        if needs_register(node) {
            let register = self.get_available_register();
            self.registers.insert(node, register);
        }
    }

    pub fn get_available_register(&mut self) -> Box<dyn Register> {
        if self.has_available_hardware_register() {
            let register = self.available_hardware_register.pop().unwrap();
            Box::new(register)
        } else {
            let register = StackRegister::new(self.current_stack_offset);
            self.current_stack_offset += 8;
            Box::new(register)
        }
    }

    pub fn has_available_hardware_register(&self) -> bool {
        !self.available_hardware_register.is_empty()
    }
}

fn needs_register(node: &Node) -> bool {
    !matches!(
        node,
        Node::Projection(_) | Node::Start(_) | Node::Block(_) | Node::Return(_)
    )
}

impl Default for RegisterAllocator<'_> {
    fn default() -> Self {
        Self::new()
    }
}
