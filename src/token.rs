use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(usize),
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    LT,
    GT,
    RETURN,
    IF,
    ELSE,
    EQ,
    NOTEQ,
    FALSE,
    TRUE,
}

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let out = match self {
            Token::ILLEGAL => "Illegal",
            Token::EOF => "",
            Token::IDENT(name) => name,
            Token::INT(number) => &number.to_string(),
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRACE => "{",
            Token::RBRACE => "}",
            Token::FUNCTION => "fn",
            Token::LET => "let",
            Token::LT => "<",
            Token::GT => ">",
            Token::RETURN => "return",
            Token::IF => "if",
            Token::EQ => "==",
            Token::NOTEQ => "!=",
            Token::FALSE => "false",
            Token::TRUE => "true",
            Token::ELSE => "else",
        };
        write!(fmt, "{}", out)
    }
}

pub fn lookup_ident(ident: &str) -> Token {
    match ident {
        "fn" => Token::FUNCTION,
        "let" => Token::LET,
        "true" => Token::TRUE,
        "false" => Token::FALSE,
        "if" => Token::IF,
        "else" => Token::ELSE,
        "return" => Token::RETURN,
        label => Token::IDENT(label.to_string()),
    }
}
