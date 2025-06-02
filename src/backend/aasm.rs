use super::regalloc::Register;

pub enum Instruction {
    Mov(Box<dyn Register>, Box<dyn Register>),
    Add(Box<dyn Register>, Box<dyn Register>),
    Sub(Box<dyn Register>, Box<dyn Register>),
    Idiv(Box<dyn Register>, Box<dyn Register>),
    Imul(Box<dyn Register>, Box<dyn Register>),
    Sall(Box<dyn Register>, Box<dyn Register>),
    Sarl(Box<dyn Register>, Box<dyn Register>),
    Jmp(String),
    Cmp(Box<dyn Register>, Box<dyn Register>),
    Test(Box<dyn Register>, Box<dyn Register>),
    Jb(String),
    Jbe(String),
    Je(String),
    Jne(String),
    Jae(String),
    Ja(String),
    Cdq,
    Leave,
    Ret,
}
