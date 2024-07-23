use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program,
        ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};

enum Precedence {
    Lowest = 1,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
}

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
            match self.parse_statement() {
                Some(stmt) => p.statements.push(stmt),
                None => {
                    println!("{}", self.curr_token.clone().expect("errr").to_string());
                    break;
                }
            }
        }
        return p;
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Some(Token::LET) => self.parse_let_statement(),
            Some(Token::RETURN) => self.parse_return_statement(),
            Some(_) => self.parse_expression_statement(),
            None => None,
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
        return Some(Statement::LetStatement(LetStatement {
            token: Token::LET,
            name,
            value: None,
        }));
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        while self.curr_token != Some(Token::SEMICOLON) {
            self.next_token();
        }
        self.next_token();
        return Some(Statement::ReturnStatement(ReturnStatement {
            token: Token::RETURN,
            return_value: None,
        }));
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt = ExpressionStatement {
            token: self.curr_token.clone()?,
            value: self.parse_expression(Precedence::Lowest)?,
        };
        if let Some(Token::SEMICOLON) = self.peek_token {
            self.next_token();
        }
        return Some(Statement::ExpressionStatement(stmt));
    }

    fn parse_expression(&self, _precedence: Precedence) -> Option<Expression> {
        println!("{}", self.curr_token.clone()?);
        match self.curr_token {
            Some(Token::IDENT(_)) => self.parse_identifier(),
            Some(Token::INT(_)) => self.parse_integer_literal(),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        println!("{}", self.curr_token.clone()?);
        Some(Expression::Identifier(Identifier {
            value: self.curr_token.clone()?.to_string(),
        }))
    }

    fn parse_integer_literal(&self) -> Option<Expression> {
        let val = self.curr_token.clone()?.to_literal();
        if let Ok(lit) = val.parse::<usize>() {
            Some(Expression::IntegerLiteral(IntegerLiteral { value: lit }))
        } else {
            None
        }
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
        match stmt {
            Statement::LetStatement(ls) => assert_eq!(ls.name.to_string(), test.to_string()),
            _ => assert!(false),
        }
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

#[test]
fn test_identifier_expression() {
    let input = "foobar;";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::ExpressionStatement(es) => match &es.value {
            Expression::Identifier(ident) => assert_eq!(ident.value, "foobar"),
            Expression::IntegerLiteral(_) => assert!(false),
        },
        _ => assert!(false),
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::ExpressionStatement(es) => match &es.value {
            Expression::IntegerLiteral(literal) => assert_eq!(literal.value, 5),
            Expression::Identifier(_) => assert!(false),
        },
        _ => assert!(false),
    }
}
