use macros_core::ebnf;
use std::io::{Cursor, Read};
use nom::character::streaming::char;
use nom::IResult as NomResult;
use nom::combinator::{map, opt};
use nom::branch::alt;
use nom::multi::many1;
use nom::sequence::tuple;
pub struct Literal {
    value: String,
}
impl From<char> for Literal {
    fn from(value: char) -> Self {
        Literal {
            value: value.to_string(),
        }
    }
}
pub struct Optional {
    value: Box<Option<Token>>,
}
impl From<Option<Token>> for Optional {
    fn from(value: Option<Token>) -> Self {
        Optional { value: Box::new(value) }
    }
}
pub struct Repetition {
    value: Vec<Token>,
}
impl From<Vec<Token>> for Repetition {
    fn from(value: Vec<Token>) -> Self {
        Repetition { value }
    }
}
pub enum Token {
    Digit(Digit),
    Digits(Digits),
    Plusminus(Plusminus),
    Integer(Integer),
    Literal(Literal),
    Optional(Optional),
    Repetition(Repetition),
}
impl Token {
    pub fn value<T>(&self) -> T {
        match self {
            _ => ::core::panicking::panic("not implemented"),
        }
    }
}
pub struct Digit {
    value: String,
}
#[automatically_derived]
impl ::core::fmt::Debug for Digit {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Digit",
            "value",
            &&self.value,
        )
    }
}
impl Digit {
    pub fn parse(input: &str) -> NomResult<&str, Token> {
        let (input, ch) = map(
            char('0'),
            |t| Token::Digit(Digit { value: t.to_string() }),
        )(input)?;
        Ok((input, ch))
    }
    pub fn value(&self) -> &String {
        &self.value
    }
}
pub struct Digits {
    value: Box<Token>,
}
impl Digits {
    pub fn parse(input: &str) -> NomResult<&str, Token> {
        let (input, parsed_alt) = map(
            alt((
                map(char('0'), |t| Token::Literal(Literal::from(t))),
                map(char('1'), |t| Token::Literal(Literal::from(t))),
            )),
            |ts| Token::Digits(Digits { value: Box::new(ts) }),
        )(input)?;
        Ok((input, parsed_alt))
    }
    pub fn value(&self) -> &Token {
        self.value.as_ref()
    }
}
pub struct Plusminus {
    value: Box<Token>,
}
impl Plusminus {
    pub fn parse(input: &str) -> NomResult<&str, Token> {
        let (input, parsed_alt) = map(
            alt((
                map(char('+'), |t| Token::Literal(Literal::from(t))),
                map(char('-'), |t| Token::Literal(Literal::from(t))),
            )),
            |ts| Token::Plusminus(Plusminus { value: Box::new(ts) }),
        )(input)?;
        Ok((input, parsed_alt))
    }
    pub fn value(&self) -> &Token {
        self.value.as_ref()
    }
}
pub struct Integer {
    value: Vec<Token>,
}
impl Integer {
    pub fn parse(input: &str) -> NomResult<&str, Token> {
        let (input, seq) = map(
            tuple((
                map(
                    opt(<Plusminus>::parse),
                    |t| Token::Optional(Optional { value: Box::new(t) }),
                ),
                <Digits>::parse,
                map(
                    many1(<Digits>::parse),
                    |ts| Token::Repetition(Repetition { value: ts }),
                ),
            )),
            |ts| {
                let mut v = Vec::with_capacity(3usize);
                v.push(ts.0);
                v.push(ts.1);
                v.push(ts.2);
                Token::Integer(Integer { value: v })
            },
        )(input)?;
        Ok((input, seq))
    }
    pub fn value(&self) -> Vec<Token> {
        ::core::panicking::panic("not implemented")
    }
}
pub fn main() {}
