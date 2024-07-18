use std::io::{self, stdin};
mod ast;
mod lexer;
mod parser;
mod token;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    println!("\r\n>>");
    while buffer.trim() != "q" {
        buffer.clear();
        stdin().read_line(&mut buffer).expect("Improper string");
        let lexer = lexer::Lexer::new(&buffer);
        for tok in lexer.into_iter() {
            println!("{}", tok.to_string());
        }
    }
    Ok(())
}
