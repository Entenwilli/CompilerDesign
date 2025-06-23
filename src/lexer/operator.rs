use std::fmt::Display;

#[derive(PartialEq, Clone, Debug)]
pub enum UnaryOperator {
    Minus,
    LogicalNot,
    BitwiseNot,
}

impl UnaryOperator {
    fn as_string(&self) -> impl ToString {
        match self {
            UnaryOperator::Minus => "-",
            UnaryOperator::LogicalNot => "!",
            UnaryOperator::BitwiseNot => "~",
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    ShiftLeft,
    ShiftRight,

    Lower,
    LowerEquals,
    Higher,
    HigherEquals,
    Equals,
    NotEquals,

    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,

    LogicalAnd,
    LogicalOr,
}

impl BinaryOperator {
    fn as_string(&self) -> impl ToString {
        match self {
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",

            BinaryOperator::Lower => "<",
            BinaryOperator::LowerEquals => "<=",
            BinaryOperator::Higher => ">",
            BinaryOperator::HigherEquals => ">=",
            BinaryOperator::Equals => "==",
            BinaryOperator::NotEquals => "!=",

            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::BitwiseOr => "|",

            BinaryOperator::LogicalAnd => "&&",
            BinaryOperator::LogicalOr => "||",
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum AssignmentOperator {
    Assign,
    AssignPlus,
    AssignMinus,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignShiftLeft,
    AssignShiftRight,
    AssignBitwiseNot,
    AssignBitwiseAnd,
    AssignBitwiseOr,
    AssignBitwiseXor,
}

impl AssignmentOperator {
    fn as_string(&self) -> impl ToString {
        match self {
            AssignmentOperator::Assign => "=",
            AssignmentOperator::AssignPlus => "+=",
            AssignmentOperator::AssignMinus => "-=",
            AssignmentOperator::AssignMul => "*=",
            AssignmentOperator::AssignDiv => "/=",
            AssignmentOperator::AssignMod => "%=",
            AssignmentOperator::AssignShiftLeft => "<<=",
            AssignmentOperator::AssignShiftRight => ">>=",
            AssignmentOperator::AssignBitwiseNot => "~=",
            AssignmentOperator::AssignBitwiseAnd => "&=",
            AssignmentOperator::AssignBitwiseOr => "|=",
            AssignmentOperator::AssignBitwiseXor => "^=",
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string().to_string())
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string().to_string())
    }
}

impl Display for AssignmentOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string().to_string())
    }
}
