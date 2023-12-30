use std::{any::type_name_of_val, fmt, str};

#[derive(Debug, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(String),
    //Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    EQ,
    NOTEQ,
    //Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Token {
    fn lookup_ident(ident: String) -> Token {
        return match ident.as_str() {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            _ => Token::IDENT(ident),
        };
    }

    pub fn to_literal(&self) -> &str {
        return match self {
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::ILLEGAL => "ILLEGAL",
            Token::EOF => "",
            Token::IDENT(name) => name.as_str(),
            Token::INT(number) => number.as_str(),
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::COMMA => ",",
            Token::SEMICOLON => todo!(),
            Token::LPAREN => todo!(),
            Token::RPAREN => todo!(),
            Token::LBRACE => todo!(),
            Token::RBRACE => todo!(),
            Token::EQ => todo!(),
            Token::NOTEQ => todo!(),
            Token::FUNCTION => todo!(),
            Token::LET => todo!(),
            Token::TRUE => todo!(),
            Token::FALSE => todo!(),
            Token::IF => todo!(),
            Token::ELSE => todo!(),
            Token::RETURN => todo!(),
        };
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token {
            Token::EOF => None,
            _ => Some(token),
        }
    }
}

impl Lexer {
    pub fn new(input_string: String) -> Self {
        let mut lex = Lexer {
            input: input_string.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lex.read_char();
        return lex;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b',' => Token::COMMA,
            b'+' => Token::PLUS,
            b'-' => Token::MINUS,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NOTEQ
                } else {
                    Token::BANG
                }
            }
            b'*' => Token::ASTERISK,
            b'/' => Token::SLASH,
            b'<' => Token::LT,
            b'>' => Token::GT,
            0 => Token::EOF,
            _ => {
                if is_letter(self.ch) {
                    return Token::lookup_ident(self.read_rest(is_letter));
                } else if is_digit(self.ch) {
                    return Token::INT(self.read_rest(is_digit));
                } else {
                    return Token::ILLEGAL;
                }
            }
        };
        self.read_char();
        return tok;
    }

    fn skip_whitespace(&mut self) {
        let whitspace_chars = [b' ', b'\t', b'\r', b'\n'];
        while whitspace_chars.contains(&self.ch) {
            self.read_char();
        }
    }

    fn read_rest(&mut self, f: fn(u8) -> bool) -> String {
        let starting_pos = self.position.clone();
        while f(self.ch) {
            self.read_char();
        }
        let s = match str::from_utf8(&self.input[starting_pos..self.position]) {
            Ok(val) => val,
            Err(e) => panic!("{}", e),
        };
        return String::from(s);
    }

    fn peek_char(&self) -> u8 {
        if self.read_position > self.input.len() {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }
}

fn is_letter(byte: u8) -> bool {
    return (b'a'..=b'z').contains(&byte) || (b'A'..=b'Z').contains(&byte);
}

fn is_digit(byte: u8) -> bool {
    return b'0' <= byte && byte <= b'9';
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_next_token() {
        let input: String = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
    return true;
} else {
    return false;
}
10 == 10;
10 != 9;"
            .to_string();

        let tests = [
            (Token::LET, "let"),
            (Token::IDENT(String::from("five")), "five"),
            (Token::ASSIGN, "="),
            (Token::INT(String::from("5")), "5"),
            (Token::SEMICOLON, ";"),
            (Token::LET, "let"),
            (Token::IDENT(String::from("ten")), "ten"),
            (Token::ASSIGN, "="),
            (Token::INT(String::from("10")), "10"),
            (Token::SEMICOLON, ";"),
            (Token::LET, "let"),
            (Token::IDENT(String::from("add")), "add"),
            (Token::ASSIGN, "="),
            (Token::FUNCTION, "fn"),
            (Token::LPAREN, "("),
            (Token::IDENT(String::from("x")), "x"),
            (Token::COMMA, ","),
            (Token::IDENT(String::from("y")), "y"),
            (Token::RPAREN, ")"),
            (Token::LBRACE, "{"),
            (Token::IDENT(String::from("x")), "x"),
            (Token::PLUS, "+"),
            (Token::IDENT(String::from("y")), "y"),
            (Token::SEMICOLON, ";"),
            (Token::RBRACE, "}"),
            (Token::SEMICOLON, ";"),
            (Token::LET, "let"),
            (Token::IDENT(String::from("result")), "result"),
            (Token::ASSIGN, "="),
            (Token::IDENT(String::from("add")), "add"),
            (Token::LPAREN, "("),
            (Token::IDENT(String::from("five")), "five"),
            (Token::COMMA, ","),
            (Token::IDENT(String::from("ten")), "ten"),
            (Token::RPAREN, ")"),
            (Token::SEMICOLON, ";"),
            (Token::BANG, "!"),
            (Token::MINUS, "-"),
            (Token::SLASH, "/"),
            (Token::ASTERISK, "*"),
            (Token::INT(String::from("5")), "5"),
            (Token::SEMICOLON, ";"),
            (Token::INT(String::from("5")), "5"),
            (Token::LT, "<"),
            (Token::INT(String::from("10")), "10"),
            (Token::GT, ">"),
            (Token::INT(String::from("5")), "5"),
            (Token::SEMICOLON, ";"),
            (Token::IF, "if"),
            (Token::LPAREN, "("),
            (Token::INT(String::from("5")), "5"),
            (Token::LT, "<"),
            (Token::INT(String::from("10")), "10"),
            (Token::RPAREN, ")"),
            (Token::LBRACE, "{"),
            (Token::RETURN, "return"),
            (Token::TRUE, "true"),
            (Token::SEMICOLON, ";"),
            (Token::RBRACE, "}"),
            (Token::ELSE, "else"),
            (Token::LBRACE, "{"),
            (Token::RETURN, "return"),
            (Token::FALSE, "false"),
            (Token::SEMICOLON, ";"),
            (Token::RBRACE, "}"),
            (Token::INT(String::from("10")), "10"),
            (Token::EQ, "=="),
            (Token::INT(String::from("10")), "10"),
            (Token::SEMICOLON, ";"),
            (Token::INT(String::from("10")), "10"),
            (Token::NOTEQ, "!="),
            (Token::INT(String::from("9")), "9"),
            (Token::SEMICOLON, ";"),
            (Token::EOF, ""),
        ];
        let mut lex = Lexer::new(input.into());

        for element in tests {
            let tok = lex.next_token();

            println!("expected: {:?}, got: {:?}", element.0, tok);

            assert!(tok == element.0);
        }
    }
}
