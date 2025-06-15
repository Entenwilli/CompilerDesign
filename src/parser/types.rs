#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Int,
    Bool,
}

impl Type {
    pub fn as_string(&self) -> &str {
        match self {
            Type::Int => "int",
            Type::Bool => "bool",
        }
    }
}
