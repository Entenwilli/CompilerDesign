use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::declaration_tree::DeclarationTree,
};

use super::IRConstructor;

impl ToIR for DeclarationTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        if let Some(initializer) = self.initializer_tree() {
            let rhs = initializer
                .to_ir(constructor)
                .expect("Expected RHS of declaration to be expression");
            constructor.write_variable(
                self.name_tree().name().clone(),
                constructor.current_block(),
                rhs,
            );
        }
        None
    }
}
