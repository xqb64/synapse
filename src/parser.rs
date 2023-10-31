use crate::tokenizer::{Token, TokenKind};
use std::collections::VecDeque;

pub struct Parser<'src> {
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    tokens: VecDeque<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Parser {
            current: None,
            previous: None,
            tokens: VecDeque::new(),
        }
    }

    pub fn parse(&mut self, tokens: VecDeque<Token<'src>>) -> Vec<Statement<'src>> {
        self.tokens = tokens;
        self.advance();
        let mut statements = vec![];
        while self.current.is_some() {
            statements.push(self.parse_statement());
        }
        statements
    }

    fn is_next(&mut self, tokens: &[TokenKind]) -> bool {
        for token in tokens {
            if self.check(*token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current.unwrap().kind == kind
    }

    fn advance(&mut self) -> Option<Token<'src>> {
        self.previous = self.current;
        self.current = self.tokens.pop_front();
        self.previous
    }

    fn consume(&mut self, kind: TokenKind) -> Option<Token<'src>> {
        if self.check(kind) {
            return self.advance();
        }
        None
    }

    fn parse_statement(&mut self) -> Statement<'src> {
        if self.is_next(&[TokenKind::Print]) {
            self.parse_print_statement()
        } else if self.is_next(&[TokenKind::Fn]) {
            self.parse_fn_statement()
        } else if self.is_next(&[TokenKind::Return]) {
            self.parse_return_statement()
        } else if self.is_next(&[TokenKind::If]) {
            self.parse_if_statement()
        } else if self.is_next(&[TokenKind::While]) {
            self.parse_while_statement()
        } else if self.is_next(&[TokenKind::LeftBrace]) {
            self.parse_block_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_print_statement(&mut self) -> Statement<'src> {
        let expression = self.parse_expression();
        self.consume(TokenKind::Semicolon);
        Statement::Print(PrintStatement { expression })
    }

    fn parse_fn_statement(&mut self) -> Statement<'src> {
        let name = self.consume(TokenKind::Identifier).unwrap();
        self.consume(TokenKind::LeftParen);
        let mut arguments = vec![];
        while !self.is_next(&[TokenKind::RightParen]) {
            let arg = self.consume(TokenKind::Identifier).unwrap();
            self.consume(TokenKind::Comma);
            arguments.push(arg.value);
        }
        self.consume(TokenKind::LeftBrace);
        let body = self.parse_block_statement();
        Statement::Fn(FnStatement {
            name: name.value,
            arguments,
            body: body.into(),
        })
    }

    fn parse_return_statement(&mut self) -> Statement<'src> {
        let expression = self.parse_expression();
        self.consume(TokenKind::Semicolon);
        Statement::Return(ReturnStatement { expression })
    }

    fn parse_if_statement(&mut self) -> Statement<'src> {
        self.consume(TokenKind::LeftParen);
        let condition = self.parse_expression();
        self.consume(TokenKind::RightParen);
        let if_branch = self.parse_statement();
        let else_branch: Statement = if self.is_next(&[TokenKind::Else]) {
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
        self.consume(TokenKind::LeftParen);
        let condition = self.parse_expression();
        self.consume(TokenKind::RightParen);
        let body = self.parse_statement();
        Statement::While(WhileStatement {
            condition,
            body: body.into(),
        })
    }

    fn parse_block_statement(&mut self) -> Statement<'src> {
        let mut body = vec![];
        while !self.is_next(&[TokenKind::RightBrace]) {
            body.push(self.parse_statement());
        }
        Statement::Block(BlockStatement { body })
    }

    fn parse_expression_statement(&mut self) -> Statement<'src> {
        let expr = self.parse_expression();
        self.consume(TokenKind::Semicolon);
        Statement::Expression(ExpressionStatement { expression: expr })
    }

    fn parse_expression(&mut self) -> Expression<'src> {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression<'src> {
        let mut result = self.equality();
        while self.is_next(&[TokenKind::Equal]) {
            result = Expression::Assign(AssignExpression {
                lhs: result.into(),
                rhs: self.equality().into(),
            })
        }
        result
    }

    fn equality(&mut self) -> Expression<'src> {
        let mut result = self.relational();
        while self.is_next(&[TokenKind::DoubleEqual, TokenKind::BangEqual]) {
            let negation = match self.previous.unwrap().kind {
                TokenKind::BangEqual => true,
                TokenKind::DoubleEqual => false,
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
            TokenKind::Less,
            TokenKind::Greater,
            TokenKind::LessEqual,
            TokenKind::GreaterEqual,
        ]) {
            let kind = match self.previous {
                Some(token) => match token.kind {
                    TokenKind::Less => BinaryExpressionKind::Less,
                    TokenKind::Greater => BinaryExpressionKind::Greater,
                    TokenKind::LessEqual => BinaryExpressionKind::LessEqual,
                    TokenKind::GreaterEqual => BinaryExpressionKind::GreaterEqual,
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
        while self.is_next(&[TokenKind::Plus, TokenKind::Minus, TokenKind::PlusPlus]) {
            let kind = match self.previous {
                Some(token) => match token.kind {
                    TokenKind::Plus => BinaryExpressionKind::Add,
                    TokenKind::Minus => BinaryExpressionKind::Sub,
                    TokenKind::PlusPlus => BinaryExpressionKind::Strcat,
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
        let mut result = self.call();
        while self.is_next(&[TokenKind::Star, TokenKind::Slash]) {
            let kind = match self.previous {
                Some(token) => match token.kind {
                    TokenKind::Star => BinaryExpressionKind::Mul,
                    TokenKind::Slash => BinaryExpressionKind::Div,
                    _ => unreachable!(),
                },
                None => unreachable!(),
            };
            result = Expression::Binary(BinaryExpression {
                kind,
                lhs: Box::new(result),
                rhs: Box::new(self.call()),
            });
        }
        result
    }

    fn call(&mut self) -> Expression<'src> {
        let mut expr = self.primary();
        if self.is_next(&[TokenKind::LeftParen]) {
            let mut arguments = vec![];
            if !self.check(TokenKind::RightParen) {
                loop {
                    arguments.push(self.parse_expression());
                    if !self.is_next(&[TokenKind::Comma]) {
                        break;
                    }
                }
            }
            self.consume(TokenKind::RightParen);
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
        if self.is_next(&[TokenKind::Number]) {
            println!("{:?}", self.previous);
            let n = self.previous.unwrap().value.parse().unwrap();
            Expression::Literal(LiteralExpression {
                value: Literal::Num(n),
            })
        } else if self.is_next(&[TokenKind::True, TokenKind::False, TokenKind::Null]) {
            let literal: Literal = self
                .previous
                .unwrap()
                .value
                .parse()
                .expect("Failed to parse a literal.");
            Expression::Literal(LiteralExpression { value: literal })
        } else if self.is_next(&[TokenKind::Identifier]) {
            let var = self.previous.unwrap().value;
            Expression::Variable(VariableExpression { value: var })
        } else if self.is_next(&[TokenKind::String]) {
            let string = self.previous.unwrap().value;
            println!("got string: {}", string);
            Expression::Literal(LiteralExpression {
                value: Literal::String(string),
            })
        } else {
            println!("{:?}", self.current);
            todo!();
        }
    }
}

impl<'src> Default for Parser<'src> {
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
    pub name: &'src str,
    pub arguments: Vec<&'src str>,
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

impl<'src> std::str::FromStr for Literal<'src> {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Literal::Bool(true)),
            "false" => Ok(Literal::Bool(false)),
            "null" => Ok(Literal::Null),
            _ => Err(format!("{} is not a valid object literal", s)),
        }
    }
}
