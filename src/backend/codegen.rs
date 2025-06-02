use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    thread::current,
};

use rand::rand_core::block;
use tracing::{debug, error, event, info, trace, Level};

use crate::ir::{
    graph::{IRGraph, END_BLOCK},
    node::{
        self, BinaryOperationData, ConditionalJumpData, ConstantBoolData, ConstantIntData, Node,
        ProjectionInformation, BINARY_OPERATION_LEFT, BINARY_OPERATION_RIGHT, COMPARISON_INDEX,
        RETURN_RESULT_INDEX,
    },
};

use super::regalloc::{HardwareRegister, Register, RegisterAllocator};

type Registers<'a> = HashMap<&'a Node, Box<dyn Register>>;

const TEMPLATE: &str = ".global main
.global _main
.text
main:
call _main
# move the return value into the first argument for the syscall
movq %rax, %rdi
# move the exit syscall number into rax
movq $0x3C, %rax
syscall
_main:
";

pub struct CodeGenerator {
    ir_graphs: Vec<IRGraph>,
    jump_label: HashMap<usize, String>,
}

impl CodeGenerator {
    pub fn new(ir_graphs: Vec<IRGraph>) -> CodeGenerator {
        CodeGenerator {
            jump_label: calculate_jump_label(&ir_graphs),
            ir_graphs,
        }
    }

    pub fn generate(self) -> String {
        let mut code = String::new();
        code.push_str(TEMPLATE);
        for ir_graph in &self.ir_graphs {
            let register_allocator = RegisterAllocator::new();
            code.push_str(&self.generate_for_graph(ir_graph, register_allocator));
        }
        code
    }

    pub fn generate_for_graph(
        &self,
        ir_graph: &IRGraph,
        register_allocator: RegisterAllocator,
    ) -> String {
        let mut visited = Vec::new();
        let (registers, stack_offset) = register_allocator.allocate_registers(ir_graph);
        let mut code = String::new();
        code.push_str("pushq %rbp\n");
        code.push_str("mov %rsp, %rbp\n");
        code.push_str(&format!("subq ${}, %rsp\n", stack_offset));

        let mut open_blocks = VecDeque::new();
        // FIXME: Can enter exit block from multiple points, therefore different points to starting
        // analysis
        let mut recorded_ends: HashMap<usize, Vec<(usize, usize)>> = HashMap::new();
        open_blocks.push_back(END_BLOCK);
        recorded_ends.insert(END_BLOCK, vec![(END_BLOCK, END_BLOCK)]);
        let mut block_codes = String::new();
        while !open_blocks.is_empty() {
            trace!("Current recorded ends: {:?}", recorded_ends);
            let current_block = open_blocks.pop_front().unwrap();
            trace!("Current block: {}", current_block);
            let mut current_block_ends = recorded_ends.remove(&current_block).unwrap();
            current_block_ends.sort_by(|(x, _), (y, _)| x.cmp(y));
            trace!("After recorded ends: {:?}", recorded_ends);
            let mut full_block_code = String::new();
            for current_block_end in current_block_ends {
                trace!(
                    "Generating Assembly for Block {}, with origins at {}",
                    current_block,
                    current_block_end.1
                );
                trace!("Current code:{}\n", block_codes);
                let (block_code, previous_blocks) = &self.generate_for_node(
                    current_block_end.0,
                    ir_graph,
                    &registers,
                    &mut visited,
                    current_block_end.1,
                );
                full_block_code.push_str(&block_code);
                info!(
                    "Generated assembly for block {:?}: {}",
                    current_block, block_code
                );
                trace!(
                    "New open blocks: {:?} with previous node {:?}",
                    previous_blocks,
                    current_block
                );
                for previous_block in previous_blocks {
                    let block_index = ir_graph.get_node(*previous_block).block();
                    if !open_blocks.contains(&block_index) {
                        open_blocks.push_back(block_index);
                    }
                    if let Some(mut old_end) = recorded_ends.remove(&block_index) {
                        old_end.push((*previous_block, current_block));
                        recorded_ends.insert(block_index, old_end);
                    } else {
                        recorded_ends.insert(block_index, vec![(*previous_block, current_block)]);
                    }
                }
            }
            block_codes = full_block_code + &block_codes;
        }
        code.push_str(&block_codes);
        code
    }

    pub fn generate_for_node(
        &self,
        node_index: usize,
        ir_graph: &IRGraph,
        registers: &Registers,
        visited: &mut Vec<usize>,
        following_block: usize,
    ) -> (String, Vec<usize>) {
        debug!(
            "Generating assembly for node {}",
            ir_graph.get_node(node_index)
        );
        let mut code = String::new();
        let mut previous_blocks = Vec::new();
        let node = ir_graph.get_node(node_index);
        if let Node::Block(data) = node {
            if let Some(label) = self.jump_label.get(&node.block()) {
                code.push_str(&format!("{}:\n", label));
            }
            debug!("Finished generating code for block: {:?}", data);
            previous_blocks.append(&mut data.predecessors().clone());
            return (code, previous_blocks);
        }
        for predecessor in node.predecessors() {
            let predecessor_block = ir_graph.get_node(*predecessor).block();
            if predecessor_block.ne(&node.block()) {
                // DO NOT ANALYZE BLOCKS NOT IN CURRENT BLOCK
                trace!(
                    "Not in current analyzed block({:?}): {:?}",
                    node.block(),
                    predecessor_block
                );
                continue;
            }
            if !visited.contains(predecessor) {
                visited.push(*predecessor);
                let (predecessor_code, predecessor_blocks) = self.generate_for_node(
                    *predecessor,
                    ir_graph,
                    registers,
                    visited,
                    following_block,
                );
                code.push_str(&predecessor_code);
                for previous_block in predecessor_blocks {
                    if !previous_blocks.contains(&previous_block) {
                        previous_blocks.push(previous_block);
                    }
                }
            }
        }
        match node {
            Node::Add(data) => {
                code.push_str(&self.generate_binary_operation(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "add",
                ));
            }
            Node::Subtraction(data) => {
                code.push_str(&self.generate_binary_operation(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "sub",
                ));
            }
            Node::Multiplication(data) => {
                code.push_str(&self.generate_binary_operation_rax(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "imul",
                    "mul",
                ));
            }
            Node::Division(data) => {
                code.push_str(&self.generate_binary_operation_rax(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "idiv",
                    "div",
                ));
            }
            Node::Modulo(data) => {
                code.push_str(&self.generate_binary_operation_rax(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "idiv",
                    "mod",
                ));
            }
            Node::Return(_) => {
                code.push_str(&self.generate_return(ir_graph, registers, node_index));
            }
            Node::ConstantInt(data) => {
                code.push_str(&self.generate_constant_int(data, ir_graph, registers, node_index));
            }
            Node::ShiftLeft(data) => {
                code.push_str(&self.generate_shift(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "sall",
                ));
            }
            Node::ShiftRight(data) => {
                code.push_str(&self.generate_shift(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "sarl",
                ));
            }
            Node::Equals(data) => {
                code.push_str(&self.generate_comparison(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                ));
            }
            Node::ConstantBool(data) => code.push_str(&self.generate_constant_bool(data.value())),
            Node::Phi(data) => {
                println!(
                    "Warning! Phi present: Aliasing {:?}",
                    data.node_data().predecessors()
                );
            }
            Node::Jump(_) => {
                trace!(
                    "Generating assembly for jump: {} with destination {}",
                    node,
                    following_block
                );
                let label = self.jump_label.get(&following_block).unwrap();
                code.push_str(&format!("jmp {}\n", label));
            }
            Node::ConditionalJump(_) => {}
            Node::Projection(data) if data.projection_info().eq(&ProjectionInformation::IfTrue) => {
                let conditional_jump_code =
                    self.generate_conditional_jump(node_index, following_block, ir_graph);
                code.push_str(&conditional_jump_code.expect("Expected jump code"));
            }
            Node::Projection(data)
                if data.projection_info().eq(&ProjectionInformation::IfFalse) =>
            {
                let jump_label = self
                    .jump_label
                    .get(&following_block)
                    .expect("Expected jump label for false if");
                code.push_str(&format!("jmp {}\n", jump_label));
            }
            Node::Block(data) => return (code, previous_blocks),
            Node::Projection(_) | Node::Start(_) => return (code, previous_blocks),
            node => panic!("unimplemented node {:?}", node),
        }
        (code, previous_blocks)
    }

    pub fn generate_constant_bool(&self, value: bool) -> String {
        let mut code = String::new();
        if value {
            code.push_str("cmp %rbx,%rbx\n");
        } else {
            code.push_str("test %rsp,%rsp\n");
        }
        code
    }

    pub fn generate_conditional_jump(
        &self,
        projection_index: usize,
        suceeding_block: usize,
        ir_graph: &IRGraph,
    ) -> Option<String> {
        let mut code = String::new();
        let true_label = self.jump_label.get(&suceeding_block).unwrap();
        let projection = ir_graph.get_node(projection_index);
        let comparision = ir_graph
            .get_node(*projection.predecessors().get(0).unwrap())
            .predecessors()
            .get(COMPARISON_INDEX)
            .unwrap();
        let op_code = match ir_graph.get_node(*comparision) {
            Node::Lower(_) => "jb",
            Node::LowerEquals(_) => "jbe",
            Node::Equals(_) => "je",
            Node::NotEquals(_) => "jne",
            Node::HigherEquals(_) => "jae",
            Node::Higher(_) => "ja",
            Node::ConstantBool(_) => "je",
            node => panic!("Invalid operation before conditional jump: {}", node),
        };
        code.push_str(&format!("{} {}\n", op_code, true_label));
        Some(code)
    }

    pub fn generate_comparison(
        &self,
        node_index: usize,
        operation_data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
    ) -> String {
        let left_value = registers
            .get(ir_graph.get_node(operation_data.left()))
            .unwrap();
        let right_value = registers
            .get(ir_graph.get_node(operation_data.right()))
            .unwrap();
        let mut code = String::new();
        //TODO: Both registers can be in memory
        code.push_str(&format!(
            "cmp {}, {}\n",
            left_value.as_assembly(),
            right_value.as_assembly()
        ));
        code
    }

    pub fn generate_binary_operation(
        &self,
        node_index: usize,
        _operation_data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let left_value = registers
            .get(ir_graph.get_node(predecessor_skip_projection(
                node_index,
                BINARY_OPERATION_LEFT,
                ir_graph,
            )))
            .unwrap();
        let right_value = registers
            .get(ir_graph.get_node(predecessor_skip_projection(
                node_index,
                BINARY_OPERATION_RIGHT,
                ir_graph,
            )))
            .unwrap();

        let destination_register = registers.get(ir_graph.get_node(node_index)).unwrap();

        let mut code = String::new();
        if !left_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&move_stack_variable(left_value));
        }
        code.push_str("mov ");
        if !left_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&HardwareRegister::Rbx.as_assembly());
        } else {
            code.push_str(&left_value.as_assembly());
        }
        code.push_str(", ");
        code.push_str(&destination_register.as_assembly());
        code.push('\n');

        if !right_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&move_stack_variable(right_value));
        }

        code.push_str(op_code);
        code.push(' ');
        if !right_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&HardwareRegister::Rbx.as_assembly());
        } else {
            code.push_str(&right_value.as_assembly());
        }
        code.push_str(", ");
        code.push_str(&destination_register.as_assembly());
        code.push('\n');
        code
    }

    pub fn generate_binary_operation_rax(
        &self,
        node_index: usize,
        _operation_data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
        op_code: &str,
        mode: &str,
    ) -> String {
        let left_value = registers
            .get(ir_graph.get_node(predecessor_skip_projection(
                node_index,
                BINARY_OPERATION_LEFT,
                ir_graph,
            )))
            .unwrap();
        let right_value = registers
            .get(ir_graph.get_node(predecessor_skip_projection(
                node_index,
                BINARY_OPERATION_RIGHT,
                ir_graph,
            )))
            .unwrap();
        let destination_register = registers.get(ir_graph.get_node(node_index)).unwrap();
        let mut code = String::new();
        code.push_str("mov $0, %rdx\n");
        code.push_str("mov $0, %rax\n");
        code.push_str("mov $0, ");
        code.push_str(&destination_register.as_assembly());
        code.push('\n');

        code.push_str("mov ");
        code.push_str(&left_value.as_32_bit_assembly());
        code.push_str(", %eax\n");

        code.push_str("CDQ\n");

        code.push_str(op_code);
        code.push(' ');
        code.push_str(&right_value.as_32_bit_assembly());
        code.push('\n');

        code.push_str("mov ");
        if mode == "mod" {
            code.push_str("%rdx");
        } else {
            code.push_str("%rax");
        }
        code.push_str(", ");
        code.push_str(&destination_register.as_assembly());
        code.push('\n');
        code
    }

    pub fn generate_shift(
        &self,
        node: usize,
        data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let mut code = String::new();
        let left_value = registers.get(ir_graph.get_node(data.left())).unwrap();
        let right_value = registers.get(ir_graph.get_node(data.right())).unwrap();
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&move_stack_variable(right_value));
            code.push_str(&format!(
                "{} {}, {}",
                op_code,
                left_value.as_assembly(),
                HardwareRegister::Rbx.as_assembly_16_bit()
            ));
        } else {
            code.push_str(&format!(
                "{} {}, {}",
                op_code,
                left_value.as_assembly(),
                right_value.as_16_bit_assembly()
            ));
        }
        let destination = registers.get(ir_graph.get_node(node)).unwrap();
        code.push_str(&format!(
            "movq {}, {}",
            left_value.as_assembly(),
            destination.as_assembly()
        ));
        code
    }

    pub fn generate_return(
        &self,
        ir_graph: &IRGraph,
        registers: &Registers,
        node_index: usize,
    ) -> String {
        let return_node_index =
            predecessor_skip_projection(node_index, RETURN_RESULT_INDEX, ir_graph);

        let mut code = String::new();
        code.push_str("mov ");
        code.push_str(
            &registers
                .get(ir_graph.get_node(return_node_index))
                .unwrap()
                .as_assembly(),
        );
        code.push_str(", %rax");
        code.push('\n');

        code.push_str("leave\n");
        code.push_str("ret\n");
        code
    }

    pub fn generate_constant_int(
        &self,
        constant_data: &ConstantIntData,
        ir_graph: &IRGraph,
        registers: &Registers,
        node_index: usize,
    ) -> String {
        let register = registers.get(ir_graph.get_node(node_index)).unwrap();

        let mut code = String::new();
        code.push_str("mov ");
        code.push_str(&(format!("$0x{:X}", &constant_data.value()).to_string()));
        code.push_str(", ");
        code.push_str(&register.as_assembly());
        code.push('\n');
        code
    }
}

fn predecessor_skip_projection(node: usize, predecessor_index: usize, graph: &IRGraph) -> usize {
    let predecessor = graph
        .get_predecessors(node)
        .get(predecessor_index)
        .expect("Invalid predecessor index");
    if let Node::Projection(data) = graph.get_node(*predecessor) {
        data.input()
    } else {
        *predecessor
    }
}

fn move_stack_variable(register: &Box<dyn Register>) -> String {
    let mut code = String::new();
    code.push_str("mov ");
    code.push_str(&register.as_assembly());
    code.push_str(", ");
    code.push_str(&HardwareRegister::Rbx.as_assembly());
    code.push('\n');
    code
}

fn calculate_jump_label(ir_graphs: &Vec<IRGraph>) -> HashMap<usize, String> {
    let mut jump_label = HashMap::new();
    let mut visited = Vec::new();
    let mut current_label = 0_usize;
    for ir_graph in ir_graphs {
        calculate_jump_label_node(
            &mut current_label,
            ir_graph,
            END_BLOCK,
            &mut jump_label,
            &mut visited,
        );
    }
    jump_label
}

fn calculate_jump_label_node<'a>(
    current_label: &mut usize,
    ir_graph: &IRGraph,
    node_index: usize,
    current: &mut HashMap<usize, String>,
    visited: &mut Vec<usize>,
) {
    let node = ir_graph.get_node(node_index);
    for predecessor in node.predecessors() {
        if !visited.contains(predecessor) {
            visited.push(*predecessor);
            calculate_jump_label_node(current_label, ir_graph, *predecessor, current, visited);
        }
    }
    if node_index == END_BLOCK {
        return;
    }
    match node {
        Node::Block(_) => {
            let block = ir_graph.get_node(node_index).block();
            current.insert(block, format!("LC{}", current_label.to_string()));
            *current_label += 1;
        }
        _ => {}
    };
}
