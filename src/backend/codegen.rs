use std::collections::HashMap;

use crate::ir::{
    graph::{IRGraph, END_BLOCK},
    node::{
        BinaryOperationData, ConstantIntData, Node, BINARY_OPERATION_LEFT, BINARY_OPERATION_RIGHT,
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
}

impl CodeGenerator {
    pub fn new(ir_graphs: Vec<IRGraph>) -> CodeGenerator {
        CodeGenerator { ir_graphs }
    }

    pub fn generate(self) -> String {
        let mut code = String::new();
        code.push_str(TEMPLATE);
        for ir_graph in self.ir_graphs.iter() {
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
        code.push_str("movq %rsp, %rbp\n");
        code.push_str(&format!("subq ${}, %rsp\n", stack_offset));
        code.push_str(&self.generate_for_node(END_BLOCK, ir_graph, &registers, &mut visited));
        code
    }

    pub fn generate_for_node(
        &self,
        node_index: usize,
        ir_graph: &IRGraph,
        registers: &Registers,
        visited: &mut Vec<usize>,
    ) -> String {
        let mut code = String::new();
        let node = ir_graph.get_node(node_index);
        for predecessor in node.predecessors() {
            if !visited.contains(predecessor) {
                visited.push(*predecessor);
                code.push_str(&self.generate_for_node(*predecessor, ir_graph, registers, visited));
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
                code.push_str(&self.generate_binary_operation(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "mul",
                ));
            }
            Node::Division(data) => {
                code.push_str(&self.generate_binary_operation_div(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "div",
                ));
            }
            Node::Modulo(data) => {
                code.push_str(&self.generate_binary_operation(
                    node_index,
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "mod",
                ));
            }
            Node::Return(_) => {
                code.push_str(&self.generate_return(ir_graph, registers, node_index));
            }
            Node::ConstantInt(data) => {
                code.push_str(&self.generate_constant_int(data, ir_graph, registers, node_index));
            }
            Node::Phi(_) => panic!("PHI present in IR Graph!"),
            Node::Block(_) | Node::Projection(_) | Node::Start(_) => return code,
        }
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
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&move_stack_variable(left_value));
        }
        code.push_str("movq ");
        if !left_value.hardware_register() && !right_value.hardware_register() {
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

    pub fn generate_binary_operation_div(
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
        code.push_str("movq ");
        code.push_str(&left_value.as_assembly());
        code.push_str(", ");
        code.push_str(&HardwareRegister::Rax.as_assembly());
        code.push('\n');

        code.push_str(op_code);
        code.push(' ');
        code.push_str(&right_value.as_assembly());
        code.push('\n');

        code.push_str("movq ");
        code.push_str(&HardwareRegister::Rax.as_assembly());
        code.push_str(", ");
        code.push_str(&destination_register.as_assembly());
        code.push('\n');
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
        code.push_str("movq ");
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
        code.push_str("movq ");
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
    code.push_str("movq ");
    code.push_str(&register.as_assembly());
    code.push_str(", ");
    code.push_str(&HardwareRegister::Rbx.as_assembly());
    code.push('\n');
    code
}
