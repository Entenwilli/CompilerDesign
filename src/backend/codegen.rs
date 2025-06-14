use std::collections::{HashMap, VecDeque};

use tracing::{debug, trace};

use crate::ir::{
    block::{Block, NodeIndex},
    graph::{BlockIndex, IRGraph, END_BLOCK},
    node::{
        binary_operation::BinaryOperationData, projection::ProjectionInformation, ConstantIntData,
        Node, ReturnData,
    },
};

use super::regalloc::{HardwareRegister, Register, RegisterAllocator};

type Registers<'a> = HashMap<(BlockIndex, NodeIndex), Box<dyn Register>>;

const TEMPLATE: &str = " .section .note.GNU-stack,\"\",@progbits
.global main
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
        let (registers, stack_offset) = register_allocator.allocate_registers(ir_graph);
        let mut code = String::new();
        code.push_str("pushq %rbp\n");
        code.push_str("mov %rsp, %rbp\n");
        code.push_str(&format!("subq ${}, %rsp\n", stack_offset));

        // Jump Information contains for each block(key) the map of exits (NodeIndex[Node that jumps], BlockIndex[Block that is jumped to])
        let mut jump_information = HashMap::new();
        let mut visited = Vec::new();
        jump_information.insert(END_BLOCK, HashMap::new());
        visited.push(END_BLOCK);
        for (block_index, block) in ir_graph.get_blocks().iter().enumerate() {
            for (previous_block_index, previous_node) in block.entry_points() {
                if !jump_information.contains_key(&previous_block_index) {
                    jump_information.insert(
                        *previous_block_index,
                        HashMap::from([(*previous_node, block_index)]),
                    );
                } else {
                    let mut existing_exits = jump_information
                        .get_mut(previous_block_index)
                        .unwrap()
                        .clone();
                    existing_exits.insert(*previous_node, block_index);
                    jump_information.insert(*previous_block_index, existing_exits);
                }
            }
        }
        code.push_str(&self.generate_recursive(
            END_BLOCK,
            ir_graph,
            &mut jump_information,
            &mut visited,
            &registers,
        ));
        code
    }

    pub fn generate_recursive(
        &self,
        block_index: BlockIndex,
        ir_graph: &IRGraph,
        jump_information: &mut HashMap<BlockIndex, HashMap<NodeIndex, BlockIndex>>,
        visited: &mut Vec<usize>,
        registers: &Registers,
    ) -> String {
        let mut code = String::new();
        let block = ir_graph.get_block(block_index);
        for (previous_block_index, _) in block.entry_points() {
            if !visited.contains(previous_block_index) {
                visited.push(*previous_block_index);
                code.push_str(&self.generate_recursive(
                    *previous_block_index,
                    ir_graph,
                    jump_information,
                    visited,
                    registers,
                ));
            }
        }
        code.push_str(&self.generate_for_block(
            block,
            block_index,
            jump_information.get(&block_index).unwrap(),
            ir_graph,
            &registers,
        ));
        code
    }

    pub fn generate_for_block(
        &self,
        block: &Block,
        block_index: BlockIndex,
        jump_information: &HashMap<NodeIndex, BlockIndex>,
        ir_graph: &IRGraph,
        registers: &Registers,
    ) -> String {
        // Start and End Nodes should not emit code
        if block.get_nodes().is_empty() {
            return String::new();
        }

        let mut code = String::new();
        let block_label = self.jump_label.get(&block_index).unwrap();
        code.push_str(&format!("{}:\n", block_label));
        for (node_index, node) in block.get_nodes().iter().enumerate() {
            code.push_str(&self.generate_for_node(
                node,
                node_index,
                block,
                block_index,
                jump_information,
                ir_graph,
                registers,
            ));
        }
        code
    }

    pub fn generate_for_node(
        &self,
        node: &Node,
        node_index: NodeIndex,
        block: &Block,
        block_index: BlockIndex,
        jump_information: &HashMap<NodeIndex, BlockIndex>,
        ir_graph: &IRGraph,
        registers: &Registers,
    ) -> String {
        debug!("Generating assembly for node {}", node);
        let mut code = String::new();
        match node {
            Node::Add(data) => {
                code.push_str(&self.generate_binary_operation(
                    node_index,
                    block,
                    block_index,
                    data,
                    ir_graph,
                    registers,
                    "add",
                ));
            }
            Node::Subtraction(data) => {
                code.push_str(&self.generate_binary_operation(
                    node_index,
                    block,
                    block_index,
                    data,
                    ir_graph,
                    registers,
                    "sub",
                ));
            }
            Node::Multiplication(data) => {
                code.push_str(&self.generate_binary_operation_rax(
                    node_index,
                    block,
                    block_index,
                    ir_graph,
                    data,
                    registers,
                    "imul",
                    "mul",
                ));
            }
            Node::Division(data) => {
                code.push_str(&self.generate_binary_operation_rax(
                    node_index,
                    block,
                    block_index,
                    ir_graph,
                    data,
                    registers,
                    "idiv",
                    "div",
                ));
            }
            Node::Modulo(data) => {
                code.push_str(&self.generate_binary_operation_rax(
                    node_index,
                    block,
                    block_index,
                    ir_graph,
                    data,
                    registers,
                    "idiv",
                    "mod",
                ));
            }
            Node::Return(data) => {
                code.push_str(&self.generate_return(ir_graph, data, registers));
            }
            Node::ConstantInt(data) => {
                code.push_str(&self.generate_constant_int(
                    data,
                    block,
                    block_index,
                    registers,
                    node_index,
                ));
            }
            Node::ShiftLeft(data) => {
                code.push_str(&self.generate_shift(
                    node_index,
                    block,
                    block_index,
                    ir_graph,
                    data,
                    registers,
                    "sall",
                ));
            }
            Node::ShiftRight(data) => {
                code.push_str(&self.generate_shift(
                    node_index,
                    block,
                    block_index,
                    ir_graph,
                    data,
                    registers,
                    "sarl",
                ));
            }
            Node::Equals(data)
            | Node::HigherEquals(data)
            | Node::LowerEquals(data)
            | Node::NotEquals(data)
            | Node::Lower(data)
            | Node::Higher(data) => {
                code.push_str(&self.generate_comparison(block, data, registers));
            }
            Node::ConstantBool(data) => code.push_str(&self.generate_constant_bool(data.value())),
            Node::Phi(data) => {
                debug!("Warning! Phi present: Aliasing {:?}", data.operands());
            }
            Node::Jump => {
                trace!(
                    "Generating assembly for jump: {} with destination XXX",
                    node,
                );
                let previous_block_index = jump_information.get(&node_index).unwrap();
                let label = self.jump_label.get(previous_block_index).unwrap();
                code.push_str(&format!("jmp {}\n", label));
            }
            Node::ConditionalJump(_) => {}
            Node::Projection(data) if data.projection_info().eq(&ProjectionInformation::IfTrue) => {
                trace!(
                    "Generating IR for true projection (including jump) with jump information {:?}",
                    jump_information
                );
                let previous_block_index = jump_information.get(&node_index).unwrap();
                let conditional_jump_code =
                    self.generate_conditional_jump(node_index, block, *previous_block_index);
                code.push_str(&conditional_jump_code.expect("Expected jump code"));
            }
            Node::Projection(data)
                if data.projection_info().eq(&ProjectionInformation::IfFalse) =>
            {
                let previous_block_index = jump_information.get(&node_index).unwrap();
                let jump_label = self
                    .jump_label
                    .get(previous_block_index)
                    .expect("Expected jump label for false if");
                code.push_str(&format!("jmp {}\n", jump_label));
            }
            Node::Projection(_) => return code,
            node => panic!("unimplemented node {:?}", node),
        }
        debug!("Generated code for IR: {}", code);
        code
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
        current_block: &Block,
        previous_block: usize,
    ) -> Option<String> {
        let mut code = String::new();
        let true_label = self.jump_label.get(&previous_block).unwrap();
        let projection = current_block.get_node(projection_index);
        let comparision = *current_block
            .get_node(*projection.predecessors().get(0).unwrap())
            .predecessors()
            .get(0)
            .unwrap();
        let op_code = match current_block.get_node(comparision) {
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
        block: &Block,
        operation_data: &BinaryOperationData,
        registers: &Registers,
    ) -> String {
        let left_value = registers.get(&operation_data.lhs()).unwrap();
        let right_value = registers.get(&operation_data.rhs()).unwrap();
        let mut code = String::new();
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&move_stack_variable(left_value));
        }
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&format!(
                "cmp {}, {}\n",
                right_value.as_assembly(),
                HardwareRegister::Rbx.as_assembly(),
            ));
        } else {
            code.push_str(&format!(
                "cmp {}, {}\n",
                right_value.as_assembly(),
                left_value.as_assembly()
            ));
        }
        code
    }

    pub fn generate_binary_operation(
        &self,
        node_index: NodeIndex,
        block: &Block,
        block_index: BlockIndex,
        data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let left_value = registers
            .get(&predecessor_skip_projection(ir_graph, data.lhs()))
            .unwrap();
        let right_value = registers
            .get(&predecessor_skip_projection(ir_graph, data.rhs()))
            .unwrap();

        let destination_register = registers.get(&(block_index, node_index)).unwrap();

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
        block: &Block,
        block_index: BlockIndex,
        ir_graph: &IRGraph,
        data: &BinaryOperationData,
        registers: &Registers,
        op_code: &str,
        mode: &str,
    ) -> String {
        let left_value = registers
            .get(&predecessor_skip_projection(ir_graph, data.lhs()))
            .unwrap();
        let right_value = registers
            .get(&predecessor_skip_projection(ir_graph, data.rhs()))
            .unwrap();
        let destination_register = registers.get(&(block_index, node_index)).unwrap();
        let mut code = String::new();
        code.push_str("movq $0, %rdx\n");
        code.push_str("movq $0, %rax\n");
        code.push_str("movq $0, ");
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
        node_index: NodeIndex,
        block: &Block,
        block_index: BlockIndex,
        ir_graph: &IRGraph,
        data: &BinaryOperationData,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let mut code = String::new();
        let left_value = registers.get(&data.lhs()).unwrap();
        let right_value = registers.get(&data.rhs()).unwrap();
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&move_stack_variable(right_value));
            code.push_str(&format!(
                "{} {}, {}\n",
                op_code,
                left_value.as_assembly(),
                HardwareRegister::Rbx.as_assembly_16_bit()
            ));
        } else {
            code.push_str(&format!(
                "{} {}, {}\n",
                op_code,
                left_value.as_assembly(),
                right_value.as_16_bit_assembly()
            ));
        }
        let destination = registers.get(&(block_index, node_index)).unwrap();
        code.push_str(&format!(
            "movq {}, {}\n",
            left_value.as_assembly(),
            destination.as_assembly()
        ));
        code
    }

    pub fn generate_return(
        &self,
        ir_graph: &IRGraph,
        data: &ReturnData,
        registers: &Registers,
    ) -> String {
        debug!("Generating assembly for return");
        let return_node = predecessor_skip_projection(ir_graph, data.input());
        debug!(
            "Determined node {} that contains the return result",
            ir_graph.get_node(return_node)
        );
        debug!("Registers: {:?}", registers);

        let mut code = String::new();
        code.push_str("mov ");
        code.push_str(&registers.get(&return_node).unwrap().as_assembly());
        code.push_str(", %rax");
        code.push('\n');

        code.push_str("leave\n");
        code.push_str("ret\n");
        code
    }

    pub fn generate_constant_int(
        &self,
        constant_data: &ConstantIntData,
        block: &Block,
        block_index: BlockIndex,
        registers: &Registers,
        node_index: NodeIndex,
    ) -> String {
        let register = registers.get(&(block_index, node_index)).unwrap();

        let mut code = String::new();
        code.push_str("movq ");
        code.push_str(&(format!("$0x{:X}", &constant_data.value()).to_string()));
        code.push_str(", ");
        code.push_str(&register.as_assembly());
        code.push('\n');
        code
    }
}

fn predecessor_skip_projection(
    ir_graph: &IRGraph,
    data: (BlockIndex, NodeIndex),
) -> (BlockIndex, NodeIndex) {
    let predecessor = ir_graph.get_node(data);
    if let Node::Projection(data) = predecessor {
        data.input()
    } else {
        data
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

fn calculate_jump_label(ir_graphs: &Vec<IRGraph>) -> HashMap<usize, String> {
    let mut jump_label = HashMap::new();
    for ir_graph in ir_graphs {
        for (block_index, block) in ir_graph.get_blocks().iter().enumerate() {
            calculate_jump_label_block(block_index, block, &mut jump_label);
        }
    }
    jump_label
}

fn calculate_jump_label_block<'a>(
    block_index: BlockIndex,
    _block: &Block,
    current: &mut HashMap<usize, String>,
) {
    current.insert(block_index, format!("LC{}", block_index));
}
