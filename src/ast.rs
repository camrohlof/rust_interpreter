use std::fmt::Display;

use crate::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            match write!(f, "{}", stmt.to_string()) {
                Err(err) => err,
                Ok(()) => continue,
            };
        }
        Ok(())
    }
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(ls) => ls.token.to_literal(),
            Statement::ReturnStatement(rs) => rs.token.to_literal(),
            Statement::ExpressionStatement(es) => es.token.to_literal(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Statement::LetStatement(ls) => ls.to_string(),
            Statement::ReturnStatement(rs) => rs.to_string(),
            Statement::ExpressionStatement(es) => es.to_string(),
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match &self.value {
            Some(item) => item.to_string(),
            None => String::from(""),
        };
        write!(
            f,
            "{} {} = {};",
            self.token.to_literal(),
            self.name.to_string(),
            val
        )
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {};",
            self.token.to_literal(),
            self.return_value.clone().unwrap().to_string(),
        )
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value.to_string())
    }
}

#[derive(Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(Box<PrefixExpression>),
    InfixExpression(Box<InfixExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id.to_string()),
            Expression::IntegerLiteral(literal) => write!(f, "{}", literal.to_string()),
            Expression::PrefixExpression(_pre) => todo!(),
            Expression::InfixExpression(_) => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub value: usize,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
}

#[derive(Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}
