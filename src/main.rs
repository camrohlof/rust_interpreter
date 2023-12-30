mod utils;
use std::io;
use utils::{lexer, repl};

fn main() -> io::Result<()> {
    println!("Welcome to the Monkey Programming Language");
    let _ = repl::start();
    Ok(())
}
