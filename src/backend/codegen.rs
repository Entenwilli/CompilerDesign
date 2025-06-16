use std::collections::HashMap;

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

pub type Registers = HashMap<(BlockIndex, NodeIndex), Box<dyn Register>>;

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
        let mut jump_information: HashMap<BlockIndex, HashMap<NodeIndex, BlockIndex>> =
            HashMap::new();
        let mut visited = Vec::new();
        jump_information.insert(END_BLOCK, HashMap::new());
        visited.push(END_BLOCK);
        for (block_index, block) in ir_graph.get_blocks().iter().enumerate() {
            for (previous_block_index, previous_nodes) in block.entry_points() {
                if !jump_information.contains_key(&previous_block_index) {
                    let mut value = HashMap::new();
                    for previous_node in previous_nodes {
                        value.insert(*previous_node, block_index);
                    }
                    jump_information.insert(*previous_block_index, value);
                } else {
                    let mut existing_exits = jump_information
                        .get_mut(previous_block_index)
                        .unwrap()
                        .clone();
                    for previous_node in previous_nodes {
                        existing_exits.insert(*previous_node, block_index);
                    }
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
                code.push_str(&self.generate_return(block, block_index, data, registers));
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
                    "sal",
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
                    "sar",
                ));
            }
            Node::Equals(_)
            | Node::HigherEquals(_)
            | Node::LowerEquals(_)
            | Node::NotEquals(_)
            | Node::Lower(_)
            | Node::Higher(_)
            | Node::LogicalNot(_)
            | Node::And(_)
            | Node::Or(_) => {}
            Node::BitwiseNegate(data) => {
                let register = registers.get(&(block_index, data.input())).unwrap();
                code.push_str(&format!("not {}\n", register.as_32_bit_assembly()));
            }
            Node::ConstantBool(data) => code.push_str(&self.generate_constant_bool(data.value())),
            Node::Phi(_) => {}
            Node::Jump => {
                trace!(
                    "Generating assembly for jump: {} with destination XXX",
                    node,
                );
                let following_block_index = jump_information.get(&node_index).unwrap();
                let label = self.jump_label.get(following_block_index).unwrap();
                code.push_str(&self.generate_phi_moves(
                    block_index,
                    *following_block_index,
                    registers,
                    ir_graph,
                ));
                code.push_str(&format!("jmp {}\n", label));
            }
            Node::ConditionalJump(_) => {}
            Node::Projection(data) if data.projection_info().eq(&ProjectionInformation::IfTrue) => {
                trace!(
                    "Generating IR for true projection (including jump) with jump information {:?}",
                    jump_information
                );
                let following_block_index = jump_information.get(&node_index).unwrap();
                let false_block_index = jump_information.get(&(node_index + 1)).unwrap();
                let conditional_jump_code = self.generate_conditional_jump_rec(
                    false,
                    node_index,
                    block,
                    block_index,
                    *following_block_index,
                    *false_block_index,
                    ir_graph,
                    registers,
                );
                code.push_str(&self.generate_phi_moves(
                    block_index,
                    *following_block_index,
                    registers,
                    ir_graph,
                ));
                code.push_str(&conditional_jump_code.expect("Expected jump code"));
            }
            Node::Projection(data)
                if data.projection_info().eq(&ProjectionInformation::IfFalse) =>
            {
                let following_block_index = jump_information.get(&node_index).unwrap();
                let jump_label = self
                    .jump_label
                    .get(following_block_index)
                    .expect("Expected jump label for false if");
                code.push_str(&self.generate_phi_moves(
                    block_index,
                    *following_block_index,
                    registers,
                    ir_graph,
                ));

                code.push_str(&format!("jmp {}\n", jump_label));
            }
            Node::Projection(_) => return code,
            node => panic!("unimplemented node {:?}", node),
        }
        debug!("Generated code for IR: {}", code);
        code
    }

    pub fn generate_phi_moves(
        &self,
        current_block_index: BlockIndex,
        following_block_index: BlockIndex,
        registers: &Registers,
        ir_graph: &IRGraph,
    ) -> String {
        let mut code = String::new();
        let following_block = ir_graph.get_block(following_block_index);
        for phi_index in following_block.phis() {
            let phi = following_block.get_node(*phi_index);
            if let Node::Phi(data) = phi {
                let destination_register =
                    registers.get(&(following_block_index, *phi_index)).unwrap();
                for operand in data.operands() {
                    if operand.0 != current_block_index {
                        continue;
                    }
                    let operand_block = ir_graph.get_block(operand.0);
                    let source_register = registers
                        .get(&(
                            operand.0,
                            predecessor_skip_projection(operand_block, operand.1),
                        ))
                        .expect(format!("Expected register for {:?}", operand).as_str());
                    if !source_register.hardware_register()
                        && !destination_register.hardware_register()
                    {
                        code.push_str(&move_stack_variable(source_register));
                        code.push_str(&format!(
                            "mov {}, {}\n",
                            HardwareRegister::Rbx.as_32_bit_assembly(),
                            destination_register.as_32_bit_assembly()
                        ));
                    } else {
                        code.push_str(&format!(
                            "mov {}, {}\n",
                            source_register.as_32_bit_assembly(),
                            destination_register.as_32_bit_assembly()
                        ));
                    }
                    break;
                }
            }
        }
        trace!(
            "Generated the following phi moves for block {}: {}",
            current_block_index,
            code
        );
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

    pub fn generate_condition_for_jump(
        &self,
        block: &Block,
        block_index: BlockIndex,
        data: &BinaryOperationData,
        ir_graph: &IRGraph,
        registers: &Registers,
    ) -> String {
        let mut code = String::new();
        code.push_str(&self.generate_comparison(block, block_index, data, ir_graph, registers));
        code
    }

    pub fn generate_conditional_jump_rec(
        &self,
        inverted: bool,
        projection_index: usize,
        current_block: &Block,
        current_block_index: BlockIndex,
        jump_block_index: usize,
        other_jump_block_index: usize,
        ir_graph: &IRGraph,
        registers: &Registers,
    ) -> Option<String> {
        let mut code = String::new();
        let projection = current_block.get_node(projection_index);
        let comparision = *current_block
            .get_node(*projection.predecessors().get(0).unwrap())
            .predecessors()
            .get(0)
            .unwrap();
        let comparison_node = current_block.get_node(comparision);
        match comparison_node {
            Node::LogicalNot(data) => {
                code.push_str(&self.generate_conditional_jump_rec(
                    !inverted,
                    data.input(),
                    current_block,
                    current_block_index,
                    jump_block_index,
                    other_jump_block_index,
                    ir_graph,
                    registers,
                )?);
            }
            Node::And(data) => {
                code.push_str(&self.generate_conditional_jump_rec(
                    inverted,
                    data.lhs(),
                    current_block,
                    current_block_index,
                    jump_block_index,
                    other_jump_block_index,
                    ir_graph,
                    registers,
                )?);
                code.push_str(&self.generate_conditional_jump_rec(
                    inverted,
                    data.rhs(),
                    current_block,
                    current_block_index,
                    jump_block_index,
                    other_jump_block_index,
                    ir_graph,
                    registers,
                )?);
            }
            Node::Or(data) => {
                code.push_str(&self.generate_conditional_jump_rec(
                    !inverted,
                    data.lhs(),
                    current_block,
                    current_block_index,
                    other_jump_block_index,
                    jump_block_index,
                    ir_graph,
                    registers,
                )?);
                code.push_str(&self.generate_conditional_jump_rec(
                    !inverted,
                    data.rhs(),
                    current_block,
                    current_block_index,
                    other_jump_block_index,
                    jump_block_index,
                    ir_graph,
                    registers,
                )?);
            }
            Node::Lower(_)
            | Node::LowerEquals(_)
            | Node::Equals(_)
            | Node::NotEquals(_)
            | Node::Higher(_)
            | Node::HigherEquals(_)
            | Node::Phi(_)
            | Node::ConstantInt(_) => {
                let op_code = if inverted {
                    self.get_inverted_jump_opcode(comparison_node)
                } else {
                    self.get_jump_opcode(comparison_node)
                };
                code.push_str(&self.generate_conditional_jump(
                    comparison_node,
                    comparision,
                    current_block,
                    current_block_index,
                    jump_block_index,
                    op_code,
                    ir_graph,
                    registers,
                )?);
            }
            _ => {}
        }
        Some(code)
    }

    pub fn get_jump_opcode(&self, comparision: &Node) -> &str {
        match comparision {
            Node::Lower(_) => "jl",
            Node::LowerEquals(_) => "jle",
            Node::Equals(_) => "je",
            Node::NotEquals(_) => "jne",
            Node::HigherEquals(_) => "jge",
            Node::Higher(_) => "jg",
            Node::ConstantBool(_) => "je",
            Node::LogicalNot(_) => "je",
            Node::Phi(_) | Node::ConstantInt(_) => "jnz",
            node => panic!("Invalid operation before conditional jump: {}", node),
        }
    }
    pub fn get_inverted_jump_opcode(&self, comparision: &Node) -> &str {
        match comparision {
            Node::Lower(_) => "jge",
            Node::LowerEquals(_) => "jg",
            Node::Equals(_) => "jne",
            Node::NotEquals(_) => "je",
            Node::HigherEquals(_) => "jl",
            Node::Higher(_) => "jle",
            Node::ConstantBool(_) => "jne",
            Node::LogicalNot(_) => "jne",
            Node::Phi(_) | Node::ConstantInt(_) => "jz",
            node => panic!("Invalid operation before conditional jump: {}", node),
        }
    }

    pub fn generate_conditional_jump(
        &self,
        comparision: &Node,
        comparision_index: NodeIndex,
        current_block: &Block,
        current_block_index: BlockIndex,
        jump_target: BlockIndex,
        op_code: &str,
        ir_graph: &IRGraph,
        registers: &Registers,
    ) -> Option<String> {
        let mut code = String::new();
        let jump_label = self.jump_label.get(&jump_target).unwrap();
        match comparision {
            Node::Lower(data)
            | Node::LowerEquals(data)
            | Node::Equals(data)
            | Node::NotEquals(data)
            | Node::Higher(data)
            | Node::HigherEquals(data) => {
                code.push_str(&self.generate_condition_for_jump(
                    current_block,
                    current_block_index,
                    data,
                    ir_graph,
                    registers,
                ));
            }
            Node::Phi(_) | Node::ConstantInt(_) => {
                let register = registers
                    .get(&(current_block_index, comparision_index))
                    .unwrap();
                code.push_str(&format!("test $0x1, {}\n", register.as_32_bit_assembly()));
            }
            _ => {}
        }
        code.push_str(&format!("{} {}\n", op_code, jump_label));
        Some(code)
    }

    pub fn generate_comparison(
        &self,
        block: &Block,
        block_index: BlockIndex,
        operation_data: &BinaryOperationData,
        _ir_graph: &IRGraph,
        registers: &Registers,
    ) -> String {
        let left_value = registers
            .get(&(
                block_index,
                predecessor_skip_projection(block, operation_data.lhs()),
            ))
            .unwrap();
        let right_value = registers
            .get(&(
                block_index,
                predecessor_skip_projection(block, operation_data.rhs()),
            ))
            .unwrap();
        let mut code = String::new();
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&move_stack_variable(left_value));
        }
        if !left_value.hardware_register() && !right_value.hardware_register() {
            code.push_str(&format!(
                "cmp {}, {}\n",
                right_value.as_32_bit_assembly(),
                HardwareRegister::Rbx.as_32_bit_assembly(),
            ));
        } else {
            code.push_str(&format!(
                "cmp {}, {}\n",
                right_value.as_32_bit_assembly(),
                left_value.as_32_bit_assembly()
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
        _ir_graph: &IRGraph,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let left_value = registers
            .get(&(block_index, predecessor_skip_projection(block, data.lhs())))
            .unwrap();
        let right_value = registers
            .get(&(block_index, predecessor_skip_projection(block, data.rhs())))
            .unwrap();

        let destination_register = registers.get(&(block_index, node_index)).unwrap();

        let mut code = String::new();
        if !left_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&move_stack_variable(left_value));
        }
        code.push_str("mov ");
        if !left_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&HardwareRegister::Rbx.as_32_bit_assembly());
        } else {
            code.push_str(&left_value.as_32_bit_assembly());
        }
        code.push_str(", ");
        code.push_str(&destination_register.as_32_bit_assembly());
        code.push('\n');

        if !right_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&move_stack_variable(right_value));
        }

        code.push_str(op_code);
        code.push(' ');
        if !right_value.hardware_register() && !destination_register.hardware_register() {
            code.push_str(&HardwareRegister::Rbx.as_32_bit_assembly());
        } else {
            code.push_str(&right_value.as_32_bit_assembly());
        }
        code.push_str(", ");
        code.push_str(&destination_register.as_32_bit_assembly());
        code.push('\n');
        code
    }

    pub fn generate_binary_operation_rax(
        &self,
        node_index: usize,
        block: &Block,
        block_index: BlockIndex,
        _ir_graph: &IRGraph,
        data: &BinaryOperationData,
        registers: &Registers,
        op_code: &str,
        mode: &str,
    ) -> String {
        let left_value = registers
            .get(&(block_index, predecessor_skip_projection(block, data.lhs())))
            .unwrap();
        let right_value = registers
            .get(&(block_index, predecessor_skip_projection(block, data.rhs())))
            .unwrap();
        let destination_register = registers.get(&(block_index, node_index)).unwrap();
        let mut code = String::new();
        code.push_str("mov $0, %rdx\n");
        code.push_str("mov $0, %rax\n");
        code.push_str("mov $0, ");
        code.push_str(&destination_register.as_32_bit_assembly());
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
            code.push_str("%edx");
        } else {
            code.push_str("%eax");
        }
        code.push_str(", ");
        code.push_str(&destination_register.as_32_bit_assembly());
        code.push('\n');
        code
    }

    pub fn generate_shift(
        &self,
        node_index: NodeIndex,
        _block: &Block,
        block_index: BlockIndex,
        _ir_graph: &IRGraph,
        data: &BinaryOperationData,
        registers: &Registers,
        op_code: &str,
    ) -> String {
        let mut code = String::new();
        let left_value = registers.get(&(block_index, data.lhs())).unwrap();
        let right_value = registers.get(&(block_index, data.rhs())).unwrap();
        code.push_str(&format!(
            "mov {}, {}\n",
            right_value.as_32_bit_assembly(),
            HardwareRegister::Rcx.as_32_bit_assembly()
        ));
        code.push_str(&format!(
            "{} {}, {}\n",
            op_code,
            HardwareRegister::Rcx.as_8_bit_assembly(),
            left_value.as_32_bit_assembly()
        ));
        let destination = registers.get(&(block_index, node_index)).unwrap();
        code.push_str(&format!(
            "mov {}, {}\n",
            left_value.as_32_bit_assembly(),
            destination.as_32_bit_assembly()
        ));
        code
    }

    pub fn generate_return(
        &self,
        block: &Block,
        block_index: BlockIndex,
        data: &ReturnData,
        registers: &Registers,
    ) -> String {
        debug!("Generating assembly for return");
        let return_node = predecessor_skip_projection(block, data.input());
        debug!(
            "Determined node {} that contains the return result",
            block.get_node(return_node)
        );
        debug!("Registers: {:?}", registers);

        let mut code = String::new();
        code.push_str("mov ");
        code.push_str(
            &registers
                .get(&(block_index, return_node))
                .unwrap()
                .as_32_bit_assembly(),
        );
        code.push_str(", %eax");
        code.push('\n');

        code.push_str("leave\n");
        code.push_str("ret\n");
        code
    }

    pub fn generate_constant_int(
        &self,
        constant_data: &ConstantIntData,
        _block: &Block,
        block_index: BlockIndex,
        registers: &Registers,
        node_index: NodeIndex,
    ) -> String {
        let register = registers.get(&(block_index, node_index)).unwrap();

        let mut code = String::new();
        code.push_str("mov ");
        code.push_str(&(format!("$0x{:X}", &constant_data.value()).to_string()));
        code.push_str(", ");
        code.push_str(&register.as_32_bit_assembly());
        code.push('\n');
        code
    }
}

fn predecessor_skip_projection(block: &Block, data: NodeIndex) -> NodeIndex {
    let predecessor = block.get_node(data);
    if let Node::Projection(data) = predecessor {
        data.input()
    } else {
        data
    }
}

fn move_stack_variable(register: &Box<dyn Register>) -> String {
    let mut code = String::new();
    code.push_str("mov ");
    code.push_str(&register.as_32_bit_assembly());
    code.push_str(", ");
    code.push_str(&HardwareRegister::Rbx.as_32_bit_assembly());
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
