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

#[derive(Clone, Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Statement::LetStatement(ls) => ls.to_string(),
            Statement::ReturnStatement(rs) => rs.to_string(),
            Statement::ExpressionStatement(es) => es.to_string(),
        };
        write!(f, "{}", out)
    }
}

#[derive(Clone, Debug)]
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
            self.token.to_string(),
            self.name.to_string(),
            val
        )
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let return_val: String;
        if let Some(exp) = self.return_value.clone() {
            return_val = exp.to_string();
        } else {
            return_val = "".to_string();
        }
        write!(f, "{} {};", self.token.to_string(), return_val,)
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value.to_string())
    }
}

#[derive(Clone, Debug)]
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
            Expression::PrefixExpression(pre) => write!(f, "{}", pre.to_string()),
            Expression::InfixExpression(inf) => write!(f, "{}", inf.to_string()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
    pub value: usize,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}
