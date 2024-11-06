use core::fmt;
use std::borrow::Cow;

use miette::{Error, LabeledSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'de> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    String(&'de str),
    Number(&'de str, f64),
    Ident(&'de str),
    And,
    Class,
    If,
    Else,
    True,
    False,
    For,
    Fun,
    Nil,
    Or,
    Return,
    Super,
    This,
    Var,
    While,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
                Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
                Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
                Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
                Token::Comma => write!(f, "COMMA , null"),
                Token::Dot => write!(f, "DOT . null"),
                Token::Minus => write!(f, "MINUS - null"),
                Token::Plus => write!(f, "PLUS + null"),
                Token::Semicolon => write!(f, "SEMICOLON ; null"),
                Token::Star => write!(f, "STAR * null"),
                Token::BangEqual => write!(f, "BANG_EQUAL != null"),
                Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
                Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
                Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
                Token::Less => write!(f, "LESS < null"),
                Token::Greater => write!(f, "GREATER > null"),
                Token::Slash => write!(f, "SLASH / null"),
                Token::String(str) => write!(f, "STRING \"{str}\" {}", Token::unescape(str)),
                Token::Number(lit, n) => write!(f,"NUMBER {lit} {n}"),
                Token::Ident(i) => write!(f, "IDENTIFIER {i} null"),
                Token::And => write!(f, "AND and null"),
                Token::Class => write!(f, "CLASS class null"),
                Token::If => write!(f, "IF if null"),
                Token::Else => write!(f, "ELSE else null"),
                Token::True => write!(f, "TRUE true null"),
                Token::False => write!(f, "FALSE false null"),
                Token::For => write!(f, "FOR for null"),
                Token::Fun => write!(f, "FUN fun null"),
                Token::Nil => write!(f, "NIL nil null"),
                Token::Or => write!(f, "OR or null"),
                Token::Return => write!(f, "RETURN return null"),
                Token::Super => write!(f, "SUPER super null"),
                Token::This => write!(f, "THIS this null"),
                Token::Var => write!(f, "VAR var null"),
                Token::While => write!(f, "WHILE while null"),
            }
    }
}

impl Token<'_>{
    pub fn unescape<'de>(str: &'de str) -> Cow<'de, str>{
        todo!()
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.byte += c.len_utf8();
        self.rest = chars.as_str();
        enum Started {
            String,
            Numbers,
            Ident
        }
        match c {
            '(' => return Some(Ok(Token::LeftParen)),
            ')' => return Some(Ok(Token::RightParen)),
            '{' => return Some(Ok(Token::LeftBrace)),
            '}' => return Some(Ok(Token::RightBrace)),
            '.' => return Some(Ok(Token::Dot)),
            ',' => return Some(Ok(Token::Comma)),
            '+' => return Some(Ok(Token::Plus)),
            '-' => return Some(Ok(Token::Minus)),
            ';' => return Some(Ok(Token::Semicolon)),
            '*' => return Some(Ok(Token::Star)),
            '"' => {}
            c => {
                return Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(self.byte - c.len_utf8()..self.byte, "this char")
                    ],
                    "Unexpected token '{c}' in input"
                }
                .with_source_code(self.whole.to_string())))
            }
        };
        todo!()
    }
}
