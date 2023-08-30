pub type Span<'i> = pest::Span<'i>;
pub type Spanned<'i, T> = (Span<'i>, T);

#[derive(Clone, Debug, PartialEq)]
pub struct Program<'i> {
    pub statements: Vec<Statement<'i>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'i> {
    Assignment(Assignment<'i>),
    PropertyDeceleration(PropertyDeceleration<'i>),
    StructDeceleration(StructDeceleration<'i>),
    IfStatement(IfStatement<'i>),
    WhileStatement(WhileStatement<'i>),
    PrintStatement(PrintStatement<'i>),
    BlockStatement(BlockStatement<'i>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'i> {
    pub lhs: Spanned<'i, String>,
    pub rhs: Expression<'i>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Qualifier {
    Var,
    Let,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PropertyDeceleration<'i> {
    pub qualifier: Qualifier,
    pub name: Spanned<'i, String>,
    pub ty: Option<Spanned<'i, String>>,
    pub initializer: Option<Expression<'i>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeceleration<'i> {
    pub name: Spanned<'i, String>,
    pub properties: Vec<PropertyDeceleration<'i>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement<'i> {
    pub condition: Spanned<'i, Expression<'i>>,
    pub if_branch: Vec<Statement<'i>>,
    pub else_branch: Vec<Statement<'i>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement<'i> {
    pub condition: Spanned<'i, Expression<'i>>,
    pub body: Vec<Statement<'i>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrintStatement<'i> {
    pub expression: Expression<'i>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement<'i> {
    pub statements: Vec<Statement<'i>>,
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
pub struct Argument<'i> {
    pub label: Option<Spanned<'i, String>>,
    pub value: Spanned<'i, Expression<'i>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'i> {
    Conditional(
        Box<Spanned<'i, Expression<'i>>>,
        Box<Expression<'i>>,
        Box<Expression<'i>>,
    ),
    Or(Box<Expression<'i>>, Span<'i>, Box<Expression<'i>>),
    And(Box<Expression<'i>>, Span<'i>, Box<Expression<'i>>),
    Equality(
        Box<Expression<'i>>,
        Spanned<'i, EqualityOperator>,
        Box<Expression<'i>>,
    ),
    Comparison(
        Box<Expression<'i>>,
        Spanned<'i, ComparisonOperator>,
        Box<Expression<'i>>,
    ),
    Term(
        Box<Expression<'i>>,
        Spanned<'i, TermOperator>,
        Box<Expression<'i>>,
    ),
    Factor(
        Box<Expression<'i>>,
        Spanned<'i, FactorOperator>,
        Box<Expression<'i>>,
    ),
    Unary(Spanned<'i, UnaryOperator>, Box<Expression<'i>>),
    Identifier(Spanned<'i, String>),
    Call(Box<Spanned<'i, Expression<'i>>>, Vec<Argument<'i>>),
    Navigation(Box<Spanned<'i, Expression<'i>>>, Spanned<'i, String>),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
}
