use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            peek_token: None,
        };
        parser.next_token().next_token();
        return parser;
    }

    fn next_token(&mut self) -> &mut Self {
        self.curr_token = self.peek_token.take();
        self.peek_token = self.lexer.next().take();

        return self;
    }

    fn expect_peek(&mut self, expected_token: Token) -> bool {
        if let Some(token) = &self.peek_token {
            if std::mem::discriminant(token) == std::mem::discriminant(&expected_token) {
                self.next_token();
                return true;
            }
        }
        return false;
    }

    pub fn parse_program(&mut self) -> Program {
        let mut p = Program { statements: vec![] };
        while self.curr_token != None {
            if let Some(stmt) = self.parse_statement() {
                p.statements.push(stmt);
            } else {
                println!("{}", self.curr_token.take().expect("errr").to_string());
            }
        }
        return p;
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Some(Token::LET) => self.parse_let_statement(),
            Some(Token::RETURN) => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(Token::IDENT(String::from(""))) {
            return None;
        }
        let name = Identifier {
            value: self.curr_token.take().unwrap().to_string(),
        };
        if !self.expect_peek(Token::ASSIGN) {
            return None;
        };
        while self.curr_token != Some(Token::SEMICOLON) {
            self.next_token();
        }
        self.next_token();
        return Some(Statement::LetStatement {
            name,
            value: Expression::InfixExpression,
        });
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let value = Expression::InfixExpression;

        while self.curr_token != Some(Token::SEMICOLON) {
            self.next_token();
        }
        self.next_token();
        return Some(Statement::ReturnStatement { value });
    }
}

#[test]
fn test_let_statements() {
    let input = "let x = 5; \
let y = 10; \
let foobar = 838383;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 3);

    let tests = ["x", "y", "foobar"];

    for index in 0..tests.len() {
        let stmt = &program.statements[index];
        let test = &tests[index];

        assert_eq!(stmt.token_literal(), "let");

        assert_eq!(
            stmt.get_name().take().unwrap().value.to_string(),
            test.to_string()
        );
    }
}

#[test]
fn test_return_statements() {
    let input = "return 5; return 10; return 993322;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 3);
}
