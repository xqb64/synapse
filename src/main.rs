use synapse::compiler::Compiler;
use synapse::parser::Parser;
use synapse::tokenizer::Tokenizer;
use synapse::vm::VM;

fn main() {
    let src = String::from("print 1 + 2;");
    let tokenizer = Tokenizer::new(&src);
    let mut parser = Parser::new();
    let mut compiler = Compiler::new();
    let ast = parser.parse(tokenizer.into_iter().collect());
    let bytecode = compiler.compile(ast);
    let mut vm = VM::new(bytecode);
    vm.run();
}
