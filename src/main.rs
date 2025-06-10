use std::{
    collections::VecDeque,
    env, fs,
    path::Path,
    process::{exit, Command},
};

use backend::codegen::CodeGenerator;
use ir::constructor::IRGraphConstructor;
use lexer::Lexer;
use parser::{ast::Tree, error::ParseError, Parser};
use rand::{distr::Alphanumeric, Rng};
use semantic::{analyze, AnalysisState};
use tracing::{debug, error, info};

pub mod backend;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod util;

fn main() {
    tracing_subscriber::fmt::init();
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        error!("Error: Invalid arguments");
        exit(3);
    }
    let input = Path::new(args.get(1).unwrap());
    let random_name = rand::rng()
        .sample_iter(&Alphanumeric)
        .take(32)
        .map(char::from)
        .collect::<String>();
    let temp = env::temp_dir().to_str().unwrap().to_owned() + "/" + &random_name + ".s";
    let output = Path::new(args.get(2).unwrap());

    let program = lex_parse(input);
    debug!("Program AST: {}", program);

    let mut state = AnalysisState::default();
    let semantic_analysis = analyze(Box::new(program.clone()), &mut state);
    if semantic_analysis.is_err() {
        error!("Semantic Error: {}", semantic_analysis.err().unwrap());
        exit(7)
    }

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
        debug!("Constructed IR: {}", ir_graph);
    }
    let code_generator = CodeGenerator::new(ir_graphs);
    let assembler = code_generator.generate();
    debug!("Assembler contents: {}", &assembler);
    fs::write(temp.clone(), assembler).expect("Filesystem: Failed to write assembler output");
    info!(
        "Filesystem: Wrote assembler to {}",
        temp.clone().to_string()
    );
    let gcc = Command::new("gcc")
        .arg(temp)
        .arg("-o")
        .arg(output)
        .output()
        .expect("Failed to invoke GCC!");
    if !gcc.stderr.as_slice().is_empty() {
        error!("{}", std::str::from_utf8(gcc.stderr.as_slice()).unwrap());
    } else {
        info!(
            "Successfully compiled {}!",
            input.file_name().unwrap().to_str().unwrap()
        )
    }
}

fn lex_parse(path: &Path) -> Tree {
    let source = fs::read_to_string(path).unwrap();
    let mut lexer = Lexer::new(source);
    let tokens = std::iter::from_fn(|| {
        let next_token = lexer.next_token();
        match next_token {
            Ok(t) => Some(t),
            Err(error) => {
                if error.eq(&ParseError::Finished) {
                    None
                } else {
                    error!("Lexing Error: {:?}", error);
                    exit(42)
                }
            }
        }
    })
    .collect::<VecDeque<_>>();
    let parser = Parser::new(tokens);
    let parse_result = parser.parse_program();
    if parse_result.is_err() && matches!(parse_result.as_ref().err().unwrap(), ParseError::Error(_))
    {
        if let ParseError::Error(error) = parse_result.as_ref().err().unwrap() {
            error!("Parsing Error {}", error);
            exit(42)
        }
    }
    parse_result.ok().unwrap()
}
