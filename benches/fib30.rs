use anyhow::{bail, Result};
use bumpalo::Bump;
use std::collections::VecDeque;

use criterion::{criterion_group, criterion_main, Criterion};
use synapse::compiler::Compiler;
use synapse::parser::Parser;
use synapse::tokenizer::{Token, Tokenizer};
use synapse::util::read_file;
use synapse::vm::VM;

fn criterion_benchmark_fib30(c: &mut Criterion) -> Result<()> {
    let arena = Bump::new();

    let path = "benches/cases/fib30.syn";
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

    c.bench_function("fib 30", |b| b.iter(|| vm.exec()));

    Ok(())
}

criterion_group!(benches, criterion_benchmark_fib30);
criterion_main!(benches);
