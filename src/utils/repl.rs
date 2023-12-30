use std::io::{self, Write};

use crate::utils::lexer::Lexer;

pub fn start() -> io::Result<()> {
    loop {
        print!(">>>");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let line = match input.trim() {
            "q" => {
                println!("Exiting...");
                break;
            }
            val => val,
        };
        for tok in Lexer::new(line.to_string()) {
            println!("Type: {}, Literal: {}", tok, tok.to_literal())
        }
    }
    Ok(())
}
