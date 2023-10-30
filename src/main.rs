use std::env;
use synapse::compiler::Compiler;
use synapse::parser::Parser;
use synapse::tokenizer::Tokenizer;
use synapse::util::read_file;
use synapse::vm::VM;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(path) => {
            let src = read_file(path)?;
            let tokenizer = Tokenizer::new(&src);
            let mut parser = Parser::default();
            let mut compiler = Compiler::default();
            let ast = parser.parse(tokenizer.into_iter().collect());
            let bytecode = compiler.compile(ast);
            let mut vm = VM::new(bytecode);
            vm.run();
        }
        None => eprintln!("You must pass in a path."),
    }

    Ok(())
}
