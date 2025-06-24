use std::{
    collections::VecDeque,
    env, fs,
    path::Path,
    process::{exit, Command},
};

use backend::codegen::CodeGenerator;
use lexer::Lexer;
use parser::{ast::Tree, error::ParseError};
use rand::{distr::Alphanumeric, Rng};
use semantic::AnalysisState;
use tracing::{debug, error, info, trace};

use crate::{
    ir::ast::{IRConstructor, ToIR},
    lexer::collection::ParserTokens,
    parser::ast::program_tree::ProgramTree,
    semantic::ast::SemanticAnalysis,
};

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
    let semantic_analysis = program.analyze(&mut state);
    if let Err(error) = semantic_analysis {
        error!("Semantic Error: {}", error);
        exit(7)
    }

    let mut ir_graphs = Vec::new();
    for function in program.functions() {
        let mut ir_graph = IRConstructor::new(function.name_tree().name().as_string().to_string());
        function.to_ir(&mut ir_graph);
        ir_graphs.push(ir_graph.graph());
    }
    for ir_graph in ir_graphs.iter().clone() {
        info!("Constructed IR: {}", ir_graph);
    }
    let code_generator = CodeGenerator::new(ir_graphs);
    let assembler = code_generator.generate();
    info!("Assembler contents: {}", &assembler);
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

fn lex_parse(path: &Path) -> ProgramTree {
    let source = fs::read_to_string(path).unwrap();
    let mut lexer = Lexer::new(source);
    let tokens = std::iter::from_fn(|| {
        let next_token = lexer.next_token();
        match next_token {
            Ok(t) => Some(t),
            Err(error) => {
                if error.eq(&ParseError::ReachedEnd) {
                    return None;
                }
                error!("Lexing Error: {:?}", error);
                exit(42)
            }
        }
    })
    .collect::<VecDeque<_>>();
    trace!("Tokens: {:?}", tokens);
    let mut parser_tokens = ParserTokens::new(tokens);
    let parse_result = ProgramTree::from_tokens(&mut parser_tokens);
    if let Err(error) = parse_result {
        error!("Parse Error: {}", error);
        exit(42)
    } else {
        parse_result.ok().unwrap()
    }
}
