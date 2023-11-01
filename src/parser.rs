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
            statements.push(self.parse_statement());
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

    fn parse_statement(&mut self) -> Statement<'src> {
        if self.is_next(&[Token::Print]) {
            self.parse_print_statement()
        } else if self.is_next(&[Token::Fn]) {
            self.parse_fn_statement()
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
            if_branch: Box::new(if_branch),
            else_branch: Box::new(else_branch),
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
                lhs: Box::new(result),
                rhs: Box::new(self.relational()),
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
                lhs: Box::new(result),
                rhs: Box::new(self.term()),
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
                lhs: Box::new(result),
                rhs: Box::new(self.factor()),
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
                lhs: Box::new(result),
                rhs: Box::new(self.unary()),
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
        }
        expr
    }

    fn primary(&mut self) -> Expression<'src> {
        if self.is_next(&[Token::Number(0.0)]) {
            if let Token::Number(n) = self.previous.unwrap() {
                Expression::Literal(LiteralExpression {
                    value: Literal::Num(n),
                })
            } else {
                unreachable!();
            }
        } else if self.is_next(&[Token::True, Token::False, Token::Null]) {
            let literal = match self.previous.unwrap() {
                Token::True => "true",
                Token::False => "false",
                Token::Null => "null",
                _ => unreachable!(),
            };
            Expression::Literal(LiteralExpression {
                value: literal.into(),
            })
        } else if self.is_next(&[Token::Identifier("")]) {
            if let Token::Identifier(var) = self.previous.unwrap() {
                Expression::Variable(VariableExpression { value: var })
            } else {
                unreachable!();
            }
        } else if self.is_next(&[Token::String("")]) {
            if let Token::String(string) = self.previous.unwrap() {
                /* The dance below is due to logos not supporting capture groups.
                 * We need to strip the quotes from the string manually. */
                Expression::Literal(LiteralExpression {
                    value: Literal::String(string),
                })
            } else {
                unreachable!();
            }
        } else {
            todo!();
        }
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

impl<'src> From<&'src str> for Literal<'src> {
    fn from(s: &'src str) -> Self {
        match s {
            "true" => Literal::Bool(true),
            "false" => Literal::Bool(false),
            "null" => Literal::Null,
            _ => unreachable!(),
        }
    }
}
