#[derive(Clone, Debug)]
pub enum Type {
    Int,
}

impl Type {
    pub fn as_string(&self) -> &str {
        match self {
            Type::Int => "int",
        }
    }
}
