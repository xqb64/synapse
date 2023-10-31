use criterion::{criterion_group, criterion_main, Criterion};
use synapse::compiler::Compiler;
use synapse::parser::Parser;
use synapse::tokenizer::Tokenizer;
use synapse::util::read_file;
use synapse::vm::VM;

fn criterion_benchmark_fib30(c: &mut Criterion) {
    let src = read_file("benches/cases/fib30.syn").unwrap();
    let tokenizer = Tokenizer::new(&src);
    let mut parser = Parser::default();
    let mut compiler = Compiler::default();
    let mut tokens = tokenizer.into_iter().collect();
    let ast = parser.parse(&mut tokens);
    let bytecode = compiler.compile(&ast);
    let mut vm = VM::new(bytecode);

    c.bench_function("fib 30", |b| b.iter(|| vm.run()));
}

criterion_group!(benches, criterion_benchmark_fib30);
criterion_main!(benches);
