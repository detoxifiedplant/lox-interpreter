use crate::{
    lex::{Eof, Token, TokenKind},
    Lexer,
};
use miette::{miette, Context, Diagnostic, Error, LabeledSpan, SourceSpan, WrapErr};
use std::{borrow::Cow, fmt};

pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
}
pub struct Ast {}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Boolean(bool),
    Ident(&'de str),
    Super,
    This,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Bang,
    Equal,
    Less,
    Greater,
    Slash,
    And,
    Or,
    If,
    For,
    Class,
    Fun,
    Print,
    Return,
    Field,
    Var,
    While,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Delemiter {
    Brace,
    Paren,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
    Fun(Atom<'de>, Vec<TokenTree<'de>>, TokenTree<'de>),
    If(TokenTree<'de>, TokenTree<'de>, Option<TokenTree<'de>>),
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_within(0)
    }

    pub fn parse_block(mut self) -> Result<TokenTree<'de>, Error> {
        self.lexer.expect(TokenKind::LeftBrace, "missing {")?;
        let block = self.parse_within(0)?;
        self.lexer.expect(TokenKind::RightBrace, "missing }")?;
        Ok(block)
    }

    pub fn parse_statement_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left hand side");
            }
        };
        let mut lhs = match lhs {
            // unary prefix expression
            Token {
                kind: TokenKind::Print | TokenKind::Return,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Print => Op::Print,
                    TokenKind::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression_within(r_bp).wrap_err("in right hand side")?;
                TokenTree::Cons(op, vec![rhs])
            }

            // prefix, two arguments
            Token {
                kind: TokenKind::Class | TokenKind::Var,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Class => Op::Class,
                    TokenKind::Var => Op::Var,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifer")
                    .wrap_err("in name of {op:?}")?;

                assert_eq!(token.kind, TokenKind::Ident);
                let ident = TokenTree::Atom(Atom::Ident(token.origin));

                if lhs.kind == TokenKind::Var {
                    self.lexer
                        .expect(TokenKind::Equal, "missing =")
                        .wrap_err("in variable assignment")?;
                }

                let second = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in second argument of {op:?}"))?;

                TokenTree::Cons(op, vec![ident, second])
            }

            // prefix, for
            Token {
                kind: TokenKind::For,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in for loop condtion")?;

                let init = self
                    .parse_expression_within(0)
                    .wrap_err("in init condition of for loop")?;

                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condtion")?;

                let condition = self
                    .parse_expression_within(0)
                    .wrap_err("in loop condition of for loop")?;

                self.lexer
                    .expect(TokenKind::Semicolon, "missing (")
                    .wrap_err("in for loop condtion")?;

                let inc = self
                    .parse_expression_within(0)
                    .wrap_err("in incremental condition of for loop")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in for loop condtion")?;

                let block = self.parse_within(0).wrap_err("block in for loop")?;

                TokenTree::Cons(Op::For, vec![init, condition, inc, block])
            }
            // prefix, while
            Token {
                kind: TokenKind::While,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in for while condtion")?;

                let condition = self
                    .parse_within(0)
                    .wrap_err("in condition of while loop")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in while while condtion")?;

                let block = self.parse_block().wrap_err("in body of while")?;

                TokenTree::Cons(Op::While, vec![condition, block])
            }

            Token {
                kind: TokenKind::Fun,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifer")
                    .wrap_err("in function name declaration")?;

                assert_eq!(token.kind, TokenKind::Ident);
                let function_name = token.origin;
                let ident = Atom::Ident(token.origin);

                let mut params = Vec::new();

                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in parameter list of function {name}")?;

                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::RightParen,
                        ..
                    }))
                ) {
                    todo!()
                } else {
                    loop {
                        let parameter = self.parse_within(0).wrap_err_with(|| {
                            format!(
                                "in parameter #{} of function {function_name}",
                                params.len() + 1
                            )
                        })?;
                        params.push(parameter);

                        self.lexer
                            .expect_where(
                                |token| {
                                    matches!(token.kind, TokenKind::RightParen | TokenKind::Comma)
                                },
                                "continuing parameter list",
                            )
                            .wrap_err_with(|| {
                                format!("in parameter list of function {function_name}")
                            })?;

                        if token.kind == TokenKind::RightParen {
                            break;
                        }
                    }
                }

                let block = self
                    .parse_block()
                    .wrap_err_with(|| format!("in body of function {function_name}"))?;

                TokenTree::Fun(ident, params, block)
            }
            Token {
                kind: TokenKind::If,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in if condtion")?;

                let condition = self.parse_within(0).wrap_err("in if loop")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in if condtion")?;

                let block = self.parse_block().wrap_err("in body of if")?;

                let mut else_body = None;
                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Else,
                        ..
                    }))
                ) {
                    self.lexer.next();
                    else_body = Some(self.parse_block().wrap_err("in body of else")?);
                }
                TokenTree::If(condition, block, else_body)
            }
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err_with(looking_for_msg);
            };
            let op = match op.map(|res| res.expect("handled Err above")) {
                None => break,
                Some(Token {
                    kind:
                        TokenKind::LeftParen
                        | TokenKind::Dot
                        | TokenKind::Minus
                        | TokenKind::Plus
                        | TokenKind::Star
                        | TokenKind::BangEqual
                        | TokenKind::EqualEqual
                        | TokenKind::LessEqual
                        | TokenKind::GreaterEqual
                        | TokenKind::Less
                        | TokenKind::Greater
                        | TokenKind::Slash
                        | TokenKind::And
                        | TokenKind::Or,
                    ..
                }) => op,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lexer.next();

                lhs = if op == '[' {
                    let rhs = self.parse_within(0);
                    assert_eq!(lexer.next(), Token::Op(']'));
                    TokenTree::Cons(op, vec![lhs, rhs])
                } else {
                    TokenTree::Cons(op, vec![lhs])
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lexer.next();

                lhs = if op == '?' {
                    let mhs = self.parse_within(0);
                    assert_eq!(lexer.next(), Token::Op(':'));
                    let rhs = self.parse_within(r_bp);
                    TokenTree::Cons(op, vec![lhs, mhs, rhs])
                } else {
                    let rhs = self.parse_within(r_bp);
                    TokenTree::Cons(op, vec![lhs, rhs])
                };
                continue;
            }

            break;
        }
        Ok(lhs)
    }

    pub fn parse_expression_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left hand side");
            }
        };
        let mut lhs = match lhs {
            Token {
                kind: TokenKind::String,
                origin,
                ..
            } => TokenTree::Atom(Atom::String(Token::unescape(origin))),
            Token {
                kind: TokenKind::Number(n),
                ..
            } => TokenTree::Atom(Atom::Number(n)),
            Token {
                kind: TokenKind::True,
                ..
            } => TokenTree::Atom(Atom::Boolean(true)),
            Token {
                kind: TokenKind::Nil,
                ..
            } => TokenTree::Atom(Atom::Nil),
            Token {
                kind: TokenKind::False,
                ..
            } => TokenTree::Atom(Atom::Boolean(false)),
            Token {
                kind: TokenKind::Ident,
                origin,
                ..
            } => TokenTree::Atom(Atom::Ident(origin)),

            // groups
            Token {
                kind: TokenKind::LeftParen | TokenKind::LeftBrace,
                ..
            } => {
                let terminator = match lhs.kind {
                    TokenKind::LeftBrace => TokenKind::RightBrace,
                    TokenKind::LeftParen => TokenKind::RightParen,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let lhs = self.parse_expression_within(0).wrap_err("in brackated expression")?;
                self.lexer
                    .expect(terminator, "Unexpected end to brackated expression")
                    .wrap_err("afer brackated expression")?;
                lhs
            }

            // unary prefix expression
            Token {
                kind: TokenKind::Bang | TokenKind::Minus,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Minus => Op::Minus,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression_within(r_bp).wrap_err("in right hand side")?;
                TokenTree::Cons(op, vec![rhs])
            }
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err_with(looking_for_msg);
            };
            let op = match op.map(|res| res.expect("handled Err above")) {
                None => break,
                Some(Token {
                    kind:
                        TokenKind::LeftParen
                        | TokenKind::Dot
                        | TokenKind::Minus
                        | TokenKind::Plus
                        | TokenKind::Star
                        | TokenKind::BangEqual
                        | TokenKind::EqualEqual
                        | TokenKind::LessEqual
                        | TokenKind::GreaterEqual
                        | TokenKind::Less
                        | TokenKind::Greater
                        | TokenKind::Slash
                        | TokenKind::And
                        | TokenKind::Or,
                    ..
                }) => op,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lexer.next();

                lhs = if op == '[' {
                    let rhs = self.parse_within(0);
                    assert_eq!(lexer.next(), Token::Op(']'));
                    TokenTree::Cons(op, vec![lhs, rhs])
                } else {
                    TokenTree::Cons(op, vec![lhs])
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lexer.next();

                lhs = if op == '?' {
                    let mhs = self.parse_within(0);
                    assert_eq!(lexer.next(), Token::Op(':'));
                    let rhs = self.parse_within(r_bp);
                    TokenTree::Cons(op, vec![lhs, mhs, rhs])
                } else {
                    let rhs = self.parse_within(r_bp);
                    TokenTree::Cons(op, vec![lhs, rhs])
                };
                continue;
            }

            break;
        }
        Ok(lhs)
    }
}

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

fn expr(input: &str) -> TokenTree {
    let mut lexer = Lexer::new(input);
    expr_bp(&mut lexer, 0)
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> TokenTree {
    lhs
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Bang | Op::Minus | Op::Return | Op::Print => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: char) -> Option<(u8, ())> {
    let res = match op {
        '!' => (11, ()),
        '[' => (11, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: char) -> Option<(u8, u8)> {
    let res = match op {
        '=' => (2, 1),
        '?' => (4, 3),
        '+' | '-' => (5, 6),
        '*' | '/' => (7, 8),
        '.' => (14, 13),
        _ => return None,
    };
    Some(res)
}
