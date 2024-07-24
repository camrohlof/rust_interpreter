use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    Lowest = 1,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn from(token: &Token) -> Precedence {
        match token {
            Token::PLUS => Precedence::Sum,
            Token::MINUS => Precedence::Sum,
            Token::ASTERISK => Precedence::Product,
            Token::SLASH => Precedence::Product,
            Token::LT => Precedence::Lessgreater,
            Token::GT => Precedence::Lessgreater,
            Token::EQ => Precedence::Equals,
            Token::NOTEQ => Precedence::Equals,
            _ => Precedence::Lowest,
        }
    }
}

struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
    infix_ops: Vec<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            peek_token: None,
            infix_ops: vec![
                Token::PLUS,
                Token::MINUS,
                Token::ASTERISK,
                Token::SLASH,
                Token::LT,
                Token::GT,
                Token::EQ,
                Token::NOTEQ,
            ],
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
            }
            self.next_token();
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
            value: self.curr_token.clone()?.to_string(),
        };
        if !self.expect_peek(Token::ASSIGN) {
            return None;
        };
        while self.curr_token != Some(Token::SEMICOLON) {
            self.next_token();
        }
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

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut leftexp = match self.curr_token.clone()? {
            Token::IDENT(_) => self.parse_identifier(),
            Token::INT(_) => self.parse_integer_literal(),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),
            _ => None,
        };
        while self
            .peek_token
            .clone()
            .is_some_and(|x| x != Token::SEMICOLON)
            && precedence < Precedence::from(&self.peek_token.clone()?)
        {
            println!("inside loop");
            if self.infix_ops.contains(&self.peek_token.clone()?) {
                self.next_token();
                dbg!(&leftexp);
                leftexp = self.parse_infix_expression(leftexp?);
                dbg!(&leftexp);
            } else {
                return leftexp;
            };
        }
        return leftexp;
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        println!("parse ident");
        let exp = Some(Expression::Identifier(Identifier {
            value: self.curr_token.clone()?.to_string(),
        }));
        return exp;
    }

    fn parse_integer_literal(&self) -> Option<Expression> {
        println!("parse literal");
        let val = self.curr_token.clone()?.to_string();
        if let Ok(lit) = val.parse::<usize>() {
            Some(Expression::IntegerLiteral(IntegerLiteral { value: lit }))
        } else {
            println!("failed to parse int");
            None
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        println!("parse prefix");
        let token = self.curr_token.clone()?;
        let operator = token.to_string();

        self.next_token();

        Some(Expression::PrefixExpression(Box::new(PrefixExpression {
            token,
            operator,
            right: self.parse_expression(Precedence::Prefix)?,
        })))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        println!("parse infix");
        let token = self.curr_token.clone()?;
        dbg!(&token);
        let operator = token.to_string();
        dbg!(&operator);
        let precedence = Precedence::from(&token);
        dbg!(&precedence);
        self.next_token();
        let right = self.parse_expression(precedence);
        dbg!(&right);
        Some(Expression::InfixExpression(Box::new(InfixExpression {
            token,
            left,
            operator,
            right: right?,
        })))
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
            _ => assert!(false),
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
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}

struct PrefixTestData {
    input: String,
    operator: String,
    integer_value: usize,
}

#[test]
fn test_parsing_prefix_expressions() {
    let input = vec![
        PrefixTestData {
            input: String::from("!5;"),
            operator: String::from("!"),
            integer_value: 5,
        },
        PrefixTestData {
            input: String::from("-15;"),
            operator: String::from("-"),
            integer_value: 15,
        },
    ];

    for case in input {
        let lexer = Lexer::new(&case.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement(es) => match &es.value {
                Expression::PrefixExpression(pe) => {
                    assert_eq!(pe.operator, case.operator);
                    match &pe.right {
                        Expression::IntegerLiteral(it) => assert_eq!(it.value, case.integer_value),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }
}

struct InfixTestData {
    input: String,
    left_value: usize,
    operator: String,
    right_value: usize,
}

#[test]
fn test_parsing_infix_expressions() {
    let input = vec![
        InfixTestData {
            input: String::from("5 + 5;"),
            left_value: 5,
            operator: String::from("+"),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 - 5;"),
            left_value: 5,
            operator: String::from("-"),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 * 5;"),
            left_value: 5,
            operator: String::from("*"),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 / 5;"),
            left_value: 5,
            operator: String::from("/"),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 > 5;"),
            left_value: 5,
            operator: String::from(">"),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 < 5;"),
            left_value: 5,
            operator: String::from("<"),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 == 5;"),
            left_value: 5,
            operator: String::from("=="),
            right_value: 5,
        },
        InfixTestData {
            input: String::from("5 != 5;"),
            left_value: 5,
            operator: String::from("!="),
            right_value: 5,
        },
    ];

    for case in input {
        let lexer = Lexer::new(&case.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement(es) => match &es.value {
                Expression::InfixExpression(ie) => {
                    match &ie.left {
                        Expression::IntegerLiteral(it) => assert_eq!(it.value, case.left_value),
                        _ => assert!(false),
                    }
                    assert_eq!(ie.operator, case.operator);
                    match &ie.right {
                        Expression::IntegerLiteral(it) => assert_eq!(it.value, case.right_value),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }
}

struct OpTestData {
    input: String,
    expected: String,
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        OpTestData {
            input: String::from("-a * b"),
            expected: String::from("((-a) * b)"),
        },
        OpTestData {
            input: String::from("!-a"),
            expected: String::from("(!(-a))"),
        },
        OpTestData {
            input: String::from("a + b + c"),
            expected: String::from("((a + b) + c)"),
        },
        OpTestData {
            input: String::from("a + b - c"),
            expected: String::from("((a + b) - c)"),
        },
        OpTestData {
            input: String::from("a * b * c"),
            expected: String::from("((a * b) * c)"),
        },
        OpTestData {
            input: String::from("a * b / c"),
            expected: String::from("((a * b) / c)"),
        },
        OpTestData {
            input: String::from("a + b / c"),
            expected: String::from("(a + (b / c))"),
        },
    ];
    for case in tests {
        let lexer = Lexer::new(&case.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let actual = program.to_string();
        assert_eq!(actual, case.expected);
    }
}
