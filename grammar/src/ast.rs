#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    PropertyDeceleration(PropertyDeceleration),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    PrintStatement(PrintStatement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Qualifier {
    Var,
    Let,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PropertyDeceleration {
    pub qualifier: Qualifier,
    pub identifier: String,
    pub ty: Option<String>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub if_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrintStatement {
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ComparisonOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TermOperator {
    Plus,
    Minus,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Not,
    Negate,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FactorOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Equality(Box<Expression>, EqualityOperator, Box<Expression>),
    Comparison(Box<Expression>, ComparisonOperator, Box<Expression>),
    Term(Box<Expression>, TermOperator, Box<Expression>),
    Factor(Box<Expression>, FactorOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Identifier(String),
    IntegerLiteral(u64),
    BooleanLiteral(bool),
    StringLiteral(String),
}
