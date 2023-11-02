use crate::bail_out;
use crate::tokenizer::Token;
use std::collections::VecDeque;

pub struct Parser<'src> {
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    tokens: Option<&'src mut VecDeque<Token<'src>>>,
}

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Parser {
            current: None,
            previous: None,
            tokens: None,
        }
    }

    pub fn parse(&mut self, tokens: &'src mut VecDeque<Token<'src>>) -> Vec<Statement<'src>> {
        self.tokens = Some(tokens);
        self.advance();
        let mut statements = vec![];
        while self.current.is_some() {
            statements.push(self.parse_declaration());
        }
        statements
    }

    fn is_next(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.check(*token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, kind: Token) -> bool {
        std::mem::discriminant(self.current.as_ref().unwrap()) == std::mem::discriminant(&kind)
    }

    fn advance(&mut self) -> Option<Token<'src>> {
        self.previous = self.current;
        self.current = self.tokens.as_mut().and_then(|tokens| tokens.pop_front());
        self.previous
    }

    fn consume(&mut self, kind: Token) -> Option<Token<'src>> {
        if self.check(kind) {
            return self.advance();
        }
        None
    }

    fn parse_declaration(&mut self) -> Statement<'src> {
        if self.is_next(&[Token::Fn]) {
            self.parse_fn_statement()
        } else if self.is_next(&[Token::Struct]) {
            self.parse_struct_statement()
        } else {
            bail_out!(parser, "expected a declaration (like 'fn' or 'struct')");
        }
    }

    fn parse_statement(&mut self) -> Statement<'src> {
        if self.is_next(&[Token::Print]) {
            self.parse_print_statement()
        } else if self.is_next(&[Token::Return]) {
            self.parse_return_statement()
        } else if self.is_next(&[Token::If]) {
            self.parse_if_statement()
        } else if self.is_next(&[Token::While]) {
            self.parse_while_statement()
        } else if self.is_next(&[Token::LeftBrace]) {
            self.parse_block_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_print_statement(&mut self) -> Statement<'src> {
        let expression = self.parse_expression();
        self.consume(Token::Semicolon);
        Statement::Print(PrintStatement { expression })
    }

    fn parse_fn_statement(&mut self) -> Statement<'src> {
        let name = self.consume(Token::Identifier("")).unwrap();
        self.consume(Token::LeftParen);
        let mut arguments = vec![];
        while !self.is_next(&[Token::RightParen]) {
            let arg = self.consume(Token::Identifier("")).unwrap();
            self.consume(Token::Comma);
            arguments.push(arg);
        }
        self.consume(Token::LeftBrace);
        let body = self.parse_block_statement();
        Statement::Fn(FnStatement {
            name,
            arguments,
            body: body.into(),
        })
    }

    fn parse_return_statement(&mut self) -> Statement<'src> {
        let expression = self.parse_expression();
        self.consume(Token::Semicolon);
        Statement::Return(ReturnStatement { expression })
    }

    fn parse_if_statement(&mut self) -> Statement<'src> {
        self.consume(Token::LeftParen);
        let condition = self.parse_expression();
        self.consume(Token::RightParen);
        let if_branch = self.parse_statement();
        let else_branch: Statement = if self.is_next(&[Token::Else]) {
            self.parse_statement()
        } else {
            Statement::Dummy
        };
        Statement::If(IfStatement {
            condition,
            if_branch: if_branch.into(),
            else_branch: else_branch.into(),
        })
    }

    fn parse_while_statement(&mut self) -> Statement<'src> {
        self.consume(Token::LeftParen);
        let condition = self.parse_expression();
        self.consume(Token::RightParen);
        let body = self.parse_statement();
        Statement::While(WhileStatement {
            condition,
            body: body.into(),
        })
    }

    fn parse_struct_statement(&mut self) -> Statement<'src> {
        let name = match self.consume(Token::Identifier("")) {
            Some(Token::Identifier(ident)) => ident,
            _ => bail_out!(
                parser,
                "expected identifier after 'struct' keyword, got: {}",
                self.current.unwrap()
            ),
        };
        self.consume(Token::LeftBrace);
        let mut members = vec![];
        while !self.is_next(&[Token::RightBrace]) {
            members.push(self.parse_struct_member());
        }
        Statement::Struct(StructStatement { name, members })
    }

    fn parse_struct_member(&mut self) -> &'src str {
        let member = match self.consume(Token::Identifier("")) {
            Some(Token::Identifier(ident)) => ident,
            _ => bail_out!(
                parser,
                "structs should be declared as: `struct s { x, y, z, }`"
            ),
        };
        self.consume(Token::Comma);
        member
    }

    fn parse_block_statement(&mut self) -> Statement<'src> {
        let mut body = vec![];
        while !self.is_next(&[Token::RightBrace]) {
            body.push(self.parse_statement());
        }
        Statement::Block(BlockStatement { body })
    }

    fn parse_expression_statement(&mut self) -> Statement<'src> {
        let expr = self.parse_expression();
        self.consume(Token::Semicolon);
        Statement::Expression(ExpressionStatement { expression: expr })
    }

    fn parse_expression(&mut self) -> Expression<'src> {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression<'src> {
        let mut result = self.equality();
        while self.is_next(&[Token::Equal]) {
            result = Expression::Assign(AssignExpression {
                lhs: result.into(),
                rhs: self.equality().into(),
            })
        }
        result
    }

    fn equality(&mut self) -> Expression<'src> {
        let mut result = self.relational();
        while self.is_next(&[Token::DoubleEqual, Token::BangEqual]) {
            let negation = match self.previous.unwrap() {
                Token::BangEqual => true,
                Token::DoubleEqual => false,
                _ => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind: BinaryExpressionKind::Equality(negation),
                lhs: result.into(),
                rhs: self.relational().into(),
            });
        }
        result
    }

    fn relational(&mut self) -> Expression<'src> {
        let mut result = self.term();
        while self.is_next(&[
            Token::Less,
            Token::Greater,
            Token::LessEqual,
            Token::GreaterEqual,
        ]) {
            let kind = match self.previous {
                Some(token) => match token {
                    Token::Less => BinaryExpressionKind::Less,
                    Token::Greater => BinaryExpressionKind::Greater,
                    Token::LessEqual => BinaryExpressionKind::LessEqual,
                    Token::GreaterEqual => BinaryExpressionKind::GreaterEqual,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: self.term().into(),
            });
        }
        result
    }

    fn term(&mut self) -> Expression<'src> {
        let mut result = self.factor();
        while self.is_next(&[Token::Plus, Token::Minus, Token::PlusPlus]) {
            let kind = match self.previous {
                Some(token) => match token {
                    Token::Plus => BinaryExpressionKind::Add,
                    Token::Minus => BinaryExpressionKind::Sub,
                    Token::PlusPlus => BinaryExpressionKind::Strcat,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: self.factor().into(),
            });
        }
        result
    }

    fn factor(&mut self) -> Expression<'src> {
        let mut result = self.unary();
        while self.is_next(&[Token::Star, Token::Slash]) {
            let kind = match self.previous {
                Some(token) => match token {
                    Token::Star => BinaryExpressionKind::Mul,
                    Token::Slash => BinaryExpressionKind::Div,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: result.into(),
                rhs: self.unary().into(),
            });
        }
        result
    }

    fn unary(&mut self) -> Expression<'src> {
        if self.is_next(&[Token::Minus, Token::Bang]) {
            let op = self.previous.unwrap();
            let expr = self.unary();
            return Expression::Unary(UnaryExpression {
                expr: expr.into(),
                op,
            });
        }
        self.call()
    }

    fn call(&mut self) -> Expression<'src> {
        let mut expr = self.primary();
        loop {
            if self.is_next(&[Token::LeftParen]) {
                let mut arguments = vec![];
                if !self.check(Token::RightParen) {
                    loop {
                        arguments.push(self.parse_expression());
                        if !self.is_next(&[Token::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(Token::RightParen);
                let name = match expr {
                    Expression::Variable(v) => v.value,
                    _ => unimplemented!(),
                };
                expr = Expression::Call(CallExpression {
                    variable: name,
                    arguments,
                });
            } else if self.is_next(&[Token::Dot]) {
                if let Some(Token::Identifier(ident)) = self.consume(Token::Identifier("")) {
                    expr = Expression::Get(GetExpression {
                        expr: expr.into(),
                        member: ident,
                    })
                };
            } else {
                break;
            }
        }
        expr
    }

    fn primary(&mut self) -> Expression<'src> {
        if self.is_next(&[Token::Number(0.0), Token::String("")]) {
            match self.previous.unwrap() {
                Token::Number(n) => self.parse_number(n),
                Token::String(s) => self.parse_string(s),
                _ => unreachable!(),
            }
        } else if self.is_next(&[Token::LeftParen]) {
            self.parse_grouping()
        } else if self.is_next(&[Token::True, Token::False, Token::Null]) {
            self.parse_literal()
        } else if self.is_next(&[Token::Identifier("")]) {
            if self.check(Token::LeftBrace) {
                self.parse_struct_expression()
            } else {
                self.parse_variable()
            }
        } else {
            bail_out!(
                parser,
                "expected: number, string, (, true, false, null, identifier"
            );
        }
    }

    fn parse_number(&mut self, n: f64) -> Expression<'src> {
        Expression::Literal(LiteralExpression { value: n.into() })
    }

    fn parse_string(&mut self, s: &'src str) -> Expression<'src> {
        Expression::Literal(LiteralExpression { value: s.into() })
    }

    fn parse_grouping(&mut self) -> Expression<'src> {
        let expr = self.parse_expression();
        self.consume(Token::RightParen);
        expr
    }

    fn parse_struct_expression(&mut self) -> Expression<'src> {
        let name = match self.previous.unwrap() {
            Token::Identifier(ident) => ident,
            _ => unreachable!(),
        };

        self.consume(Token::LeftBrace);

        let mut initializers = vec![];
        while !self.is_next(&[Token::RightBrace]) {
            initializers.push(self.parse_struct_initializer());
            self.consume(Token::Comma);
        }

        Expression::Struct(StructExpression { name, initializers })
    }

    fn parse_struct_initializer(&mut self) -> Expression<'src> {
        let member = self.parse_expression();
        self.consume(Token::Colon);
        let value = self.parse_expression();

        Expression::StructInitializer(StructInitializerExpression {
            member: member.into(),
            value: value.into(),
        })
    }

    fn parse_variable(&mut self) -> Expression<'src> {
        match self.previous {
            Some(Token::Identifier(ident)) => {
                Expression::Variable(VariableExpression { value: ident })
            }
            _ => unreachable!(),
        }
    }

    fn parse_literal(&mut self) -> Expression<'src> {
        let literal = match self.previous.unwrap() {
            Token::True => Literal::Bool(true),
            Token::False => Literal::Bool(false),
            Token::Null => Literal::Null,
            _ => unreachable!(),
        };
        Expression::Literal(LiteralExpression { value: literal })
    }
}

impl Default for Parser<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum Statement<'src> {
    Print(PrintStatement<'src>),
    Fn(FnStatement<'src>),
    Return(ReturnStatement<'src>),
    If(IfStatement<'src>),
    While(WhileStatement<'src>),
    Struct(StructStatement<'src>),
    Block(BlockStatement<'src>),
    Expression(ExpressionStatement<'src>),
    Dummy,
}

#[derive(Debug)]
pub struct PrintStatement<'src> {
    pub expression: Expression<'src>,
}

#[derive(Debug)]
pub struct FnStatement<'src> {
    pub name: Token<'src>,
    pub arguments: Vec<Token<'src>>,
    pub body: Box<Statement<'src>>,
}

#[derive(Debug)]
pub struct ReturnStatement<'src> {
    pub expression: Expression<'src>,
}

#[derive(Debug)]
pub struct IfStatement<'src> {
    pub condition: Expression<'src>,
    pub if_branch: Box<Statement<'src>>,
    pub else_branch: Box<Statement<'src>>,
}

#[derive(Debug)]
pub struct WhileStatement<'src> {
    pub condition: Expression<'src>,
    pub body: Box<Statement<'src>>,
}

#[derive(Debug)]
pub struct StructStatement<'src> {
    pub name: &'src str,
    pub members: Vec<&'src str>,
}

#[derive(Debug)]
pub struct BlockStatement<'src> {
    pub body: Vec<Statement<'src>>,
}

#[derive(Debug)]
pub struct ExpressionStatement<'src> {
    pub expression: Expression<'src>,
}

#[derive(Debug)]
pub enum Expression<'src> {
    Literal(LiteralExpression<'src>),
    Variable(VariableExpression<'src>),
    Binary(BinaryExpression<'src>),
    Call(CallExpression<'src>),
    Assign(AssignExpression<'src>),
    Unary(UnaryExpression<'src>),
    Get(GetExpression<'src>),
    Struct(StructExpression<'src>),
    StructInitializer(StructInitializerExpression<'src>),
}

#[derive(Debug)]
pub struct LiteralExpression<'src> {
    pub value: Literal<'src>,
}

#[derive(Debug)]
pub struct VariableExpression<'src> {
    pub value: &'src str,
}

#[derive(Debug)]
pub struct BinaryExpression<'src> {
    pub kind: BinaryExpressionKind,
    pub lhs: Box<Expression<'src>>,
    pub rhs: Box<Expression<'src>>,
}

#[derive(Debug)]
pub struct CallExpression<'src> {
    pub variable: &'src str,
    pub arguments: Vec<Expression<'src>>,
}

#[derive(Debug)]
pub struct AssignExpression<'src> {
    pub lhs: Box<Expression<'src>>,
    pub rhs: Box<Expression<'src>>,
}

#[derive(Debug)]
pub struct UnaryExpression<'src> {
    pub expr: Box<Expression<'src>>,
    pub op: Token<'src>,
}

#[derive(Debug)]
pub struct StructExpression<'src> {
    pub name: &'src str,
    pub initializers: Vec<Expression<'src>>,
}

#[derive(Debug)]
pub struct StructInitializerExpression<'src> {
    pub member: Box<Expression<'src>>,
    pub value: Box<Expression<'src>>,
}

#[derive(Debug)]
pub struct GetExpression<'src> {
    pub expr: Box<Expression<'src>>,
    pub member: &'src str,
}

#[derive(Debug)]
pub enum BinaryExpressionKind {
    Add,
    Sub,
    Mul,
    Div,
    Equality(bool), /* negation */
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Strcat,
}

#[derive(Debug)]
pub enum Literal<'src> {
    Num(f64),
    String(&'src str),
    Bool(bool),
    Null,
}

impl<'src> From<f64> for Literal<'src> {
    fn from(value: f64) -> Self {
        Self::Num(value)
    }
}

impl<'src> From<&'src str> for Literal<'src> {
    fn from(value: &'src str) -> Self {
        Self::String(value)
    }
}
