use crate::{parser::types::Type, util::span::Span};

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum KeywordType {
    Struct,
    If,
    Else,
    While,
    For,
    Continue,
    Break,
    Return,
    Assert,
    True,
    False,
    Null,
    Print,
    Read,
    Alloc,
    AllocArray,
    Int,
    Bool,
    Void,
    Char,
    String,
}

impl KeywordType {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Struct => "struct",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::For => "for",
            Self::Continue => "continue",
            Self::Break => "break",
            Self::Return => "return",
            Self::Assert => "assert",
            Self::True => "true",
            Self::False => "false",
            Self::Null => "null",
            Self::Print => "print",
            Self::Read => "read",
            Self::Alloc => "alloc",
            Self::AllocArray => "alloc_array",
            Self::Int => "int",
            Self::Bool => "bool",
            Self::Void => "void",
            Self::Char => "char",
            Self::String => "string",
        }
    }

    pub fn from_string(keyword: &str) -> Option<Self> {
        match keyword {
            "struct" => Some(Self::Struct),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "while" => Some(Self::While),
            "for" => Some(Self::For),
            "continue" => Some(Self::Continue),
            "break" => Some(Self::Break),
            "return" => Some(Self::Return),
            "assert" => Some(Self::Assert),
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "null" => Some(Self::Null),
            "print" => Some(Self::Print),
            "read" => Some(Self::Read),
            "alloc" => Some(Self::Alloc),
            "alloc_array" => Some(Self::AllocArray),
            "int" => Some(Self::Int),
            "bool" => Some(Self::Bool),
            "void" => Some(Self::Void),
            "char" => Some(Self::Char),
            "string" => Some(Self::String),
            _ => None,
        }
    }

    pub fn is_control_keyword(&self) -> bool {
        matches!(
            self,
            Self::If | Self::While | Self::For | Self::Continue | Self::Break | Self::Return
        )
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Self::Bool | Self::Int)
    }
}

#[derive(Eq, Hash, Clone, Debug, PartialEq)]
pub enum OperatorType {
    AssignMinus,
    Minus,
    AssignPlus,
    Plus,
    AssignMul,
    Mul,
    AssignDiv,
    Div,
    AssignMod,
    Mod,
    LogicalNot,
    BitwiseNot,
    AssignBitwiseNot,
    ShiftLeft,
    AssignShiftLeft,
    ShiftRight,
    AssignShiftRight,
    Lower,
    LowerEquals,
    Higher,
    HigherEquals,
    Equals,
    NotEquals,
    BitwiseAnd,
    AssignBitwiseAnd,
    BitwiseXor,
    AssignBitwiseXor,
    BitwiseOr,
    AssignBitwiseOr,
    LogicalAnd,
    LogicalOr,
    TernaryQuestionMark,
    TernaryColon,
    Assign,
}

pub const MAX_PRECEDENCE: u8 = 13;

impl OperatorType {
    pub fn is_assignment_operator(&self) -> bool {
        matches!(
            self,
            Self::Assign
                | Self::AssignMul
                | Self::AssignDiv
                | Self::AssignMod
                | Self::AssignPlus
                | Self::AssignMinus
                | Self::AssignBitwiseNot
                | Self::AssignShiftLeft
                | Self::AssignShiftRight
                | Self::AssignBitwiseAnd
                | Self::AssignBitwiseXor
                | Self::AssignBitwiseOr
        )
    }

    pub fn get_precedence(&self) -> Vec<u8> {
        match self {
            Self::LogicalNot | Self::BitwiseNot => vec![1],
            Self::Minus => vec![1, 3],
            Self::Mul | Self::Div | Self::Mod => vec![2],
            Self::Plus => vec![3],
            Self::ShiftLeft | Self::ShiftRight => vec![4],
            Self::Lower | Self::LowerEquals | Self::Higher | Self::HigherEquals => vec![5],
            Self::Equals | Self::NotEquals => vec![6],
            Self::BitwiseAnd => vec![7],
            Self::BitwiseXor => vec![8],
            Self::BitwiseOr => vec![9],
            Self::LogicalAnd => vec![10],
            Self::LogicalOr => vec![11],
            Self::TernaryQuestionMark | Self::TernaryColon => vec![12],
            operator if operator.is_assignment_operator() => vec![13],
            _ => panic!("Attempted to get predecence of operator {:?}", self),
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::AssignMinus => "-=",
            Self::Minus => "-",
            Self::AssignPlus => "+=",
            Self::Plus => "+",
            Self::AssignMul => "*=",
            Self::Mul => "*",
            Self::AssignDiv => "/=",
            Self::Div => "/",
            Self::AssignMod => "%=",
            Self::Mod => "%",
            Self::LogicalNot => "!",
            Self::BitwiseNot => "~",
            Self::AssignBitwiseNot => "~=",
            Self::ShiftLeft => "<<",
            Self::AssignShiftLeft => "<<=",
            Self::ShiftRight => ">>",
            Self::AssignShiftRight => ">>=",
            Self::Lower => "<",
            Self::LowerEquals => "<=",
            Self::Higher => ">",
            Self::HigherEquals => ">=",
            Self::Equals => "==",
            Self::NotEquals => "!=",
            Self::BitwiseAnd => "&",
            Self::AssignBitwiseAnd => "&=",
            Self::BitwiseXor => "^",
            Self::AssignBitwiseXor => "^=",
            Self::BitwiseOr => "|",
            Self::AssignBitwiseOr => "|=",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::TernaryQuestionMark => "?",
            Self::TernaryColon => ":",
            Self::Assign => "=",
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum SeperatorType {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
}

impl SeperatorType {
    fn as_str(&self) -> &str {
        match self {
            Self::ParenOpen => "(",
            Self::ParenClose => ")",
            Self::BraceOpen => "{",
            Self::BraceClose => "}",
            Self::Semicolon => ";",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Token {
    ErrorToken(Span, String),
    Identifier(Span, String),
    Keyword(Span, KeywordType),
    NumberLiteral(Span, String, u64),
    BoolLiteral(Span, String),
    Operator(Span, OperatorType),
    Separator(Span, SeperatorType),
}

impl Token {
    pub fn span(self) -> Span {
        match self {
            Self::ErrorToken(span, _) => span,
            Self::Identifier(span, _) => span,
            Self::Keyword(span, _) => span,
            Self::NumberLiteral(span, _, _) => span,
            Self::BoolLiteral(span, _) => span,
            Self::Operator(span, _) => span,
            Self::Separator(span, _) => span,
        }
    }
    pub fn is_keyword(&self, other_keyword_type: &KeywordType) -> bool {
        match self {
            Self::Keyword(_, keyword_type) => keyword_type.eq(other_keyword_type),
            _ => false,
        }
    }

    pub fn is_control_keyword(&self) -> bool {
        match self {
            Self::Keyword(_, keyword_type) => keyword_type.is_control_keyword(),
            _ => false,
        }
    }

    pub fn is_type_keyword(&self) -> bool {
        match self {
            Self::Keyword(_, keyword_type) => keyword_type.is_type(),
            _ => false,
        }
    }

    pub fn get_type_value(&self) -> Option<Type> {
        match self {
            Self::Keyword(_, keyword_type) if keyword_type.is_type() => match keyword_type {
                KeywordType::Int => Some(Type::Int),
                KeywordType::Bool => Some(Type::Bool),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_operator(&self) -> bool {
        match self {
            Self::Operator(_, _) => true,
            _ => false,
        }
    }

    pub fn is_operator_type(&self, other_operator: &OperatorType) -> bool {
        match self {
            Self::Operator(_, operator) => operator.eq(other_operator),
            _ => false,
        }
    }

    pub fn is_assignment_operator(&self) -> bool {
        matches!(self, Self::Operator(_, operator) if operator.is_assignment_operator())
    }

    pub fn is_separator(&self, other_seperator: &SeperatorType) -> bool {
        match self {
            Self::Separator(_, seperator) => seperator.eq(other_seperator),
            _ => false,
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            Self::ErrorToken(_, value) => value.as_str(),
            Self::Identifier(_, value) => value.as_str(),
            Self::Keyword(_, keyword) => keyword.as_str(),
            Self::NumberLiteral(_, value, _) => value.as_str(),
            Self::BoolLiteral(_, value) => value.as_str(),
            Self::Operator(_, operator) => operator.as_str(),
            Self::Separator(_, seperator) => seperator.as_str(),
        }
    }
}
