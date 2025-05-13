use std::{
    collections::VecDeque,
    env, fs,
    path::Path,
    process::{exit, Command},
};

use backend::codegen::CodeGenerator;
use ir::constructor::IRGraphConstructor;
use lexer::Lexer;
use parser::{ast::Tree, Parser};
use semantic::{analyze, AnalysisState};

pub mod backend;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod util;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        println!("Error: Invalid arguments");
        exit(3);
    }
    let input = Path::new(args.get(1).unwrap());
    let output = Path::new(args.get(2).unwrap());

    let program = lex_parse(input);
    println!("Parsed into AST: {}\n", program);
    let mut state = AnalysisState::default();
    analyze(Box::new(program.clone()), &mut state);
    let mut ir_graphs = Vec::new();
    if let Tree::Program(functions) = program {
        for function in functions {
            let mut ir_graph = IRGraphConstructor::new();
            ir_graph.convert(function);
            ir_graphs.push(ir_graph.graph());
        }
    } else {
        panic!("Result from parser is not a program tree!");
    }
    for ir_graph in ir_graphs.iter().clone() {
        println!("Constructed IR:\n{}", ir_graph);
    }
    let code_generator = CodeGenerator::new(ir_graphs);
    fs::write("temp.s", code_generator.generate())
        .expect("Filesystem: Failed to write assembler output");
    Command::new("gcc")
        .arg("temp.s")
        .arg("-o")
        .arg(output)
        .output()
        .expect("Failed to invoke GCC!");
}

fn lex_parse(path: &Path) -> Tree {
    let source = fs::read_to_string(path).unwrap();
    let mut lexer = Lexer::new(source);
    let tokens = std::iter::from_fn(|| lexer.next_token()).collect::<VecDeque<_>>();
    let parser = Parser::new(tokens);
    parser.parse_program()
}
