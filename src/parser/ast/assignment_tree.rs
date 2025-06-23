use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{collection::ParserTokens, operator::AssignmentOperator},
    parser::{
        ast::{expression_tree::ExpressionTree, lvalue_tree::LValueTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct AssignmentTree {
    lvalue: LValueTree,
    operator: AssignmentOperator,
    expression: ExpressionTree,
}

impl Tree for AssignmentTree {
    fn span(&self) -> Span {
        self.lvalue.span().merge(&self.expression.span())
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing assignment from tokens");
        trace!("First token {:?}", tokens.peek());
        let lvalue = LValueTree::from_tokens(tokens)?;
        let operator = tokens.expect_assignment_operator()?;
        let expression = ExpressionTree::from_tokens(tokens)?;
        Ok(AssignmentTree {
            lvalue,
            operator,
            expression,
        })
    }
}

impl AssignmentTree {
    pub fn lvalue(&self) -> &LValueTree {
        &self.lvalue
    }

    pub fn operator(&self) -> &AssignmentOperator {
        &self.operator
    }

    pub fn expression(&self) -> &ExpressionTree {
        &self.expression
    }
}

impl Display for AssignmentTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {} {}", self.lvalue, self.operator, self.expression)
    }
}
