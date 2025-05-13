use std::collections::HashMap;

use crate::ir::{
    graph::{IRGraph, END_BLOCK},
    node::{BinaryOperationData, ConstantIntData, Node, ReturnData, RETURN_RESULT_INDEX},
};

use super::regalloc::{Register, RegisterAllocator};

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

    pub fn generate(mut self) -> String {
        let mut code = String::new();
        code.push_str(TEMPLATE);
        for (index, ir_graph) in self.ir_graphs.iter().enumerate() {
            let register_allocator = RegisterAllocator::new();
            let registers = register_allocator.allocate_registers(&ir_graph);
            code.push_str(&self.generate_for_graph(ir_graph, registers));
        }
        code
    }

    pub fn generate_for_graph(&self, ir_graph: &IRGraph, registers: Registers) -> String {
        let mut visited = Vec::new();
        self.generate_for_node(END_BLOCK, ir_graph, &registers, &mut visited)
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
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "add",
                ));
            }
            Node::Subtraction(data) => {
                code.push_str(&self.generate_binary_operation(
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "sub",
                ));
            }
            Node::Multiplication(data) => {
                code.push_str(&self.generate_binary_operation(
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "mul",
                ));
            }
            Node::Division(data) => {
                code.push_str(&self.generate_binary_operation(
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "div",
                ));
            }
            Node::Modulo(data) => {
                code.push_str(&self.generate_binary_operation(
                    data.binary_operation_data(),
                    ir_graph,
                    registers,
                    "mod",
                ));
            }
            Node::Return(data) => {
                code.push_str(&self.generate_return(data, ir_graph, registers, node_index));
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
        operation_data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let mut code = String::new();
        code.push_str(op_code);
        todo!("Actually generate code");
        code
    }

    pub fn generate_return(
        &self,
        return_data: &ReturnData,
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
        code.push_str(", %rax\n");
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
