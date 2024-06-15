use anyhow::{bail, Result};
use bumpalo::Bump;
use std::collections::VecDeque;
use std::env;
use synapse::compiler::Compiler;
use synapse::parser::Parser;
use synapse::tokenizer::{Token, Tokenizer};
use synapse::util::read_file;
use synapse::vm::VM;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(path) => {
            if let Err(e) = run(path) {
                eprintln!("synapse: {:?}", e);
            }
        }
        None => eprintln!("synapse: You must pass in a path."),
    }
}

fn run(path: &str) -> Result<()> {
    let arena = Bump::new();

    let src = arena.alloc_str(&read_file(path)?);

    let mut tokenizer = Tokenizer::new(src);
    let mut parser = Parser::default();
    let mut compiler = Compiler::new(&arena, path);

    let Some(tokens) = tokenizer
        .by_ref()
        .map(|token| {
            if token != Token::Error {
                Some(token)
            } else {
                None
            }
        })
        .collect::<Option<VecDeque<Token<'_>>>>()
    else {
        let unrecognized = tokenizer.get_lexer().slice();
        bail!("tokenizer: unexpected token: {}", unrecognized);
    };

    let ast = parser.parse(tokens)?;
    let bytecode = compiler.compile(&ast)?;

    let mut vm = VM::new(bytecode);
    vm.exec()?;

    Ok(())
}
