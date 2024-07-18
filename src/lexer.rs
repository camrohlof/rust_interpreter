use crate::token::{lookup_ident, Token};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let charlist: Vec<char> = input.chars().collect();
        let mut lex = Lexer {
            input: charlist.clone(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        lex.read_char();
        return lex;
    }

    fn read_char(&mut self) -> &Self {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input[self.read_position])
        }
        self.position = self.read_position;
        self.read_position += 1;
        return self;
    }

    fn next_token(&mut self) -> Token {
        let mut tok: Token = Token::EOF;
        if let Some(ch) = self.skip_whitespace().ch {
            tok = match ch {
                '=' => {
                    if self.peek_char().is_some_and(|x| x == '=') {
                        self.read_char();
                        Token::EQ
                    } else {
                        Token::ASSIGN
                    }
                }
                ';' => Token::SEMICOLON,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                '{' => Token::LBRACE,
                '}' => Token::RBRACE,
                '+' => Token::PLUS,
                '-' => Token::MINUS,
                ',' => Token::COMMA,
                '!' => {
                    if self.peek_char().is_some_and(|x| x == '=') {
                        self.read_char();
                        Token::NOTEQ
                    } else {
                        Token::BANG
                    }
                }
                '/' => Token::SLASH,
                '*' => Token::ASTERISK,
                '<' => Token::LT,
                '>' => Token::GT,
                _ => {
                    if ch.is_alphabetic() {
                        return lookup_ident(&self.read_identifier());
                    } else if ch.is_numeric() {
                        return Token::INT(self.read_number());
                    } else {
                        Token::ILLEGAL
                    }
                }
            }
        }

        self.read_char();
        return tok;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_some_and(|x| x.is_alphabetic()) {
            self.read_char();
        }
        return self.input[pos..self.position].into_iter().collect();
    }

    fn read_number(&mut self) -> usize {
        let pos = self.position;
        while self.ch.is_some_and(|x| x.is_numeric()) {
            self.read_char();
        }
        let literal: String = self.input[pos..self.position].into_iter().collect();
        return literal.parse::<usize>().unwrap();
    }

    fn skip_whitespace(&mut self) -> &Self {
        let whitespaces = vec![' ', '\t', '\n', '\r'];
        while self.ch.is_some_and(|x| whitespaces.contains(&x)) {
            self.read_char();
        }
        return self;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            return None;
        } else {
            return Some(self.input[self.read_position]);
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        match tok {
            Token::EOF => None,
            _ => Some(tok),
        }
    }
}

#[test]
fn test_next_token() {
    let input = "let five = 5; \
let ten = 10; \
\
let add = fn(x, y) { \
  x + y; \
}; \
\
let result = add(five, ten); \
!-/*5; \
5 < 10 > 5; \
\
if (5 < 10) { \
	return true; \
} else { \
	return false; \
} \
\
10 == 10; \
10 != 9;";

    let tests = vec![
        (Token::LET, "let"),
        (Token::IDENT("five".to_string()), "five"),
        (Token::ASSIGN, "="),
        (Token::INT("5".parse().expect("Not a number")), "5"),
        (Token::SEMICOLON, ";"),
        (Token::LET, "let"),
        (Token::IDENT("ten".to_string()), "ten"),
        (Token::ASSIGN, "="),
        (Token::INT("10".parse().expect("Not a number")), "10"),
        (Token::SEMICOLON, ";"),
        (Token::LET, "let"),
        (Token::IDENT("add".to_string()), "add"),
        (Token::ASSIGN, "="),
        (Token::FUNCTION, "fn"),
        (Token::LPAREN, "("),
        (Token::IDENT("x".to_string()), "x"),
        (Token::COMMA, ","),
        (Token::IDENT("y".to_string()), "y"),
        (Token::RPAREN, ")"),
        (Token::LBRACE, "{"),
        (Token::IDENT("x".to_string()), "x"),
        (Token::PLUS, "+"),
        (Token::IDENT("y".to_string()), "y"),
        (Token::SEMICOLON, ";"),
        (Token::RBRACE, "}"),
        (Token::SEMICOLON, ";"),
        (Token::LET, "let"),
        (Token::IDENT("result".to_string()), "result"),
        (Token::ASSIGN, "="),
        (Token::IDENT("add".to_string()), "add"),
        (Token::LPAREN, "("),
        (Token::IDENT("five".to_string()), "five"),
        (Token::COMMA, ","),
        (Token::IDENT("ten".to_string()), "ten"),
        (Token::RPAREN, ")"),
        (Token::SEMICOLON, ";"),
        (Token::BANG, "!"),
        (Token::MINUS, "-"),
        (Token::SLASH, "/"),
        (Token::ASTERISK, "*"),
        (Token::INT(5), "5"),
        (Token::SEMICOLON, ";"),
        (Token::INT(5), "5"),
        (Token::LT, "<"),
        (Token::INT(10), "10"),
        (Token::GT, ">"),
        (Token::INT(5), "5"),
        (Token::SEMICOLON, ";"),
        (Token::IF, "if"),
        (Token::LPAREN, "("),
        (Token::INT(5), "5"),
        (Token::LT, "<"),
        (Token::INT(10), "10"),
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
        (Token::INT(10), "10"),
        (Token::EQ, "=="),
        (Token::INT(10), "10"),
        (Token::SEMICOLON, ";"),
        (Token::INT(10), "10"),
        (Token::NOTEQ, "!="),
        (Token::INT(9), "9"),
        (Token::SEMICOLON, ";"),
        (Token::EOF, ""),
    ];

    let mut lexer = Lexer::new(input);

    for test in tests {
        let token = lexer.next_token();

        assert_eq!(test.0, token);
        assert_eq!(test.1, token.to_literal());
    }
}
