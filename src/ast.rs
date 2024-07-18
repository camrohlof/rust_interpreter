pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    LetStatement { name: Identifier, value: Expression },
    ReturnStatement { value: Expression },
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement { name: _, value: _ } => {
                format!("let")
            }
            Statement::ReturnStatement { value: _ } => format!("return"),
        }
    }

    pub fn get_value(&self) -> Expression {
        match self {
            Statement::LetStatement { name: _, value } => value.clone(),
            Statement::ReturnStatement { value } => value.clone(),
        }
    }

    pub fn get_name(&self) -> Option<Identifier> {
        match self {
            Statement::LetStatement { name, value: _ } => Some(name.clone()),
            Statement::ReturnStatement { value: _ } => None,
        }
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub value: String,
}

#[derive(Clone)]
pub enum Expression {
    PrefixExpression,
    InfixExpression,
    PostfixExpression,
}

impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::PrefixExpression => format!("Prefix"),
            Expression::InfixExpression => format!("Infix"),
            Expression::PostfixExpression => format!("Postfix"),
        }
    }
}
