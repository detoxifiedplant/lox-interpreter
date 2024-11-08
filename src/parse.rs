use crate::{
    lex::{Token, TokenKind},
    Lexer,
};
use miette::{miette, Context, Diagnostic, Error, LabeledSpan, SourceSpan, WrapErr};
use std::{borrow::Cow, fmt};
use thiserror::Error;

pub struct Parser<'de> {
    whole: &'de str,
    lexer: std::iter::Peekable<Lexer<'de>>,
}
pub struct Ast {}

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

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
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_within(0)
    }

    pub fn parse_within(
        &mut self,
        looking_for: Option<(Op, usize)>,
        min_bp: u8,
    ) -> Result<TokenTree<'de>, Error> {
        let looking_for_msg = move || {
            if let Some((op, argi)) = looking_for {
                format!("argument #{argi} for {op:?}")
            } else {
                "a statement".to_string()
            }
        };
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err_with(looking_for_msg);
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
                let lhs = self
                    .parse_within(looking_for, 0)
                    .wrap_err("in brackated expression")?;
                match self.lexer.next() {
                    Some(Ok(token)) if token.kind == terminator => {}
                    Some(Ok(token)) => {
                        return Err(miette::miette!{
                            labels = vec![
                                LabeledSpan::at(token.offset..token.offset + token.origin.len(), "this {terminator:?}"),
                            ],
                            help = "Expected {terminator:?}",
                            "Unexpected end to brackated expression"
                            }.with_source_code(self.whole.to_string())).wrap_err_with(looking_for_msg);
                    }
                    Some(Err(e)) => return Err(e).wrap_err_with(looking_for_msg),
                    None => return Err(Eof).wrap_err_with(looking_for_msg),
                }
                // assert_eq!(self.lexer.next().kind, terminator);
                lhs
            }

            // prefix, two arguments
            Token {
                kind: TokenKind::For | TokenKind::While | TokenKind::Class,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::For => Op::For,
                    TokenKind::While => Op::While,
                    TokenKind::Class => Op::Class,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let first = self
                    .parse_within(Some((op, 0)), 0)
                    .wrap_err_with(|| format!("in {op:?} expression"))?;
                let second = self
                    .parse_within(Some((op, 1)), 0)
                    .wrap_err_with(|| format!("in {op:?} expression"))?;

                TokenTree::Cons(op, vec![first,second])
            }

            // prefix expression
            Token {
                kind: TokenKind::Bang | TokenKind::Print | TokenKind::Minus | TokenKind::Return,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Print => Op::Print,
                    TokenKind::Minus => Op::Minus,
                    TokenKind::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_within(Some((op, 0)), r_bp)
                    .wrap_err("parse RHS")?;
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
