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
            Token::ILLEGAL => "Illegal: {}",
            Token::EOF => "End of File: {}",
            Token::IDENT(name) => name,
            Token::INT(number) => &format!("Int: {}", number),
            Token::ASSIGN => "Assign: {}",
            Token::PLUS => "Plus: {}",
            Token::MINUS => "Minus: {}",
            Token::BANG => "Bang: {}",
            Token::ASTERISK => "Asterisk: {}",
            Token::SLASH => "Slash: {}",
            Token::COMMA => "Comma: {}",
            Token::SEMICOLON => "Semicolon: {}",
            Token::LPAREN => "LParen: {}",
            Token::RPAREN => "RParen: {}",
            Token::LBRACE => "LBrace: {}",
            Token::RBRACE => "RBrace: {}",
            Token::FUNCTION => "Function: {}",
            Token::LET => "Let: {}",
            Token::LT => "LessThan: {}",
            Token::GT => "GreaterThan: {}",
            Token::RETURN => "Return: {}",
            Token::IF => "If: {}",
            Token::EQ => "EQ: {}",
            Token::NOTEQ => "Not EQ: {}",
            Token::FALSE => "FALSE",
            Token::TRUE => "TRUE",
            Token::ELSE => "Else: {}",
        };

        fmt.write_str(out)?;

        Ok(())
    }
}

impl Token {
    pub fn to_literal(&self) -> String {
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
        return out.to_string();
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
