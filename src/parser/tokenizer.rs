use std::ops::Index;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while_m_n},
    character::{
        complete::{
            alpha1, alphanumeric1, char, digit1, line_ending, multispace0, not_line_ending, one_of,
        },
        is_alphabetic,
    },
    combinator::{cut, map, map_res, not, opt},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{length_count, many0, many_m_n, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};

const RESERVED: &[&str] = &[
    "and", "false", "local", "then", "break", "for", "nil", "true", "do", "function", "not",
    "until", "else", "goto", "or", "while", "elseif", "if", "repeat", "end", "in", "return",
];

#[derive(Debug)]
pub enum TokenizerError {
    ReservedWordUsed(String),
}

impl std::fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cause = match self {
            TokenizerError::ReservedWordUsed(s) => format!("reserved word used: {}", s),
        };
        write!(f, "{}", cause)
    }
}

impl std::error::Error for TokenizerError {}

#[derive(Debug)]
struct Identifier {
    s: String,
}

enum Token {
    Identifier(Identifier),
}

fn identifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Identifier, E> {
    let (i, s) = context(
        "identifier_start",
        one_of("_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
    )(i)?;
    let (i, rest) = context("identifier_rest", many0(alt((alphanumeric1, tag("_")))))(i)?;
    let ident = format!("{}{}", s, rest.into_iter().collect::<String>());
    if RESERVED.contains(&ident.as_str()) {
        // i dont know how to trigger this error
        let _ = context("identifier_kw", cut(tag("fail")))("not")?;
        unreachable!()
    }
    Ok((i, Identifier { s: ident }))
}

#[derive(strum::FromRepr)]
enum Operator {
    Plus = 0,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Circumflex,       // ^
    Hashtag,          // # (?)
    And,              // &
    Tilde,            // ~
    Wall,             // |
    DoubleLess,       // <<
    DoubleGreater,    // >>
    DoubleSlash,      // //
    DoubleEqual,      // ==
    AlmostEqual,      // ~=
    LessEqual,        // <=
    GreaterEqual,     // >=
    Less,             // <
    Greater,          // >
    Equal,            // =
    OpenParentheses,  // (
    CloseParentheses, // )
    OpenCurly,        // {
    CloseCurly,       // }
    OpenSquare,       // [
    CloseSquare,      // ]
    DoubleColon,      // ::
    Semicolon,        // ;
    Colon,            // :
    Comma,            // ,
    FullStop,         // .
    DoubleFullStop,   // ..
    Elipsis,          // ...
}

const OPERATOR_SYMB: &[&str] = &[
    "+", "-", "*", "/", "%", "^", "#", "&", "~", "|", "<<", ">>", "//", "==", "~=", "<=", ">=",
    "<", ">", "=", "(", ")", "{", "}", "[", "]", "::", ";", ":", ",", ".", "..", "...",
];

fn operator<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Operator, E> {
    let (i, op) = context(
        "operator",
        alt((
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
            tag("^"),
            tag("#"),
            tag("&"),
            tag("~"),
            tag("|"),
            tag("<<"),
            tag(">>"),
            tag("//"),
            alt((
                tag("=="),
                tag("~="),
                tag("<="),
                tag(">="),
                tag("<"),
                tag(">"),
                tag("="),
                tag("("),
                tag(")"),
                tag("{"),
                tag("}"),
                tag("["),
                tag("]"),
                tag("::"),
                tag(";"),
                tag(":"),
                tag(","),
                tag("."),
                tag(".."),
                tag("..."),
            )),
        )),
    )(i)?;
    if let Some(idx) = OPERATOR_SYMB.iter().position(|e| *e == op) {
        Ok((i, Operator::from_repr(idx).unwrap()))
    } else {
        let _ = context("operator_err", cut(tag("false")))("true")?;
        unreachable!()
    }
}

enum ShortStringLiteralContent {}

fn short_string_literal_content<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, String, E> {
    let escape_seq = tuple((tag("\\"), one_of("abfnrtv\\\"\'")));
    let hex_byte = tuple((tag("\\x"), take_while_m_n(2, 2, |c: char| c.is_digit(16))));
    let digits = tuple((tag("\\"), take_while_m_n(1, 3, |c: char| c.is_digit(10))));
    let utf_char = tuple((tag("\\u"), take_while_m_n(3, 3, |c: char| c.is_digit(16))));
    let (i, res) = context(
        "short_string_literal_content",
        alt((
            map(escape_seq, |(c, c2)| (c, c2.to_string())),
            map(hex_byte, |(c, c2): (&str, &str)| (c, c2.to_string())),
            map(digits, |(c, c2): (&str, &str)| (c, c2.to_string())),
            map(utf_char, |(c, c2): (&str, &str)| (c, c2.to_string())),
        )),
    )(i)?;
    Ok((i, format!("{}{}", res.0, res.1)))
}

#[derive(Debug, PartialEq)]
struct ShortLiteralString(String);

fn short_literal_string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ShortLiteralString, E> {
    let single = context(
        "short_literal_string_single",
        delimited(char('\''), short_string_literal_content, char('\'')),
    );
    let double = context(
        "short_literal_string_double",
        delimited(char('\"'), short_string_literal_content, char('\"')),
    );
    let (i, lit) = context("short_literal_string", alt((single, double)))(i)?;
    Ok((i, ShortLiteralString(lit)))
}

struct LiteralString;

// fn literal_string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
//     i: &'a str,
// ) -> IResult<&'a str, LiteralString, E> {
// }

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::{convert_error, VerboseError};

    #[test]
    fn check_identifier() -> Result<(), Box<dyn std::error::Error>> {
        let test_idents = &[
            ("check", true),
            ("and", false),
            ("_123", true),
            ("123asd", false),
            ("a123_", true),
            ("And", true),
            ("AND", true),
        ];
        for t in test_idents {
            let ident = identifier::<VerboseError<&str>>(t.0);
            if t.1 && ident.is_err() {
                let s = match &ident {
                    Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                        convert_error(t.0, e.clone())
                    }
                    _ => unreachable!(),
                };
                println!("nom error: {}", s);
                let _ = ident?;
                unreachable!()
            }
            if ident.is_ok() {
                if t.1 {
                    let n = ident?;
                    assert_eq!(n.1.s, t.0);
                } else {
                    let n = ident?;
                    assert!(false, "ident is wrong {:?}", n);
                }
            }
        }

        Ok(())
    }

    #[test]
    fn check_operator() {
        for op in OPERATOR_SYMB {
            match operator::<VerboseError<&str>>(op) {
                Ok(_) => continue,
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    println!("op error: {}", convert_error(*op, e.clone()));
                    assert!(false);
                }
                _ => {
                    assert!(false);
                }
            };
        }
        assert!(operator::<VerboseError<&str>>("asd").is_err());
        assert!(operator::<VerboseError<&str>>("123").is_err());
        assert!(operator::<VerboseError<&str>>("_dsa").is_err());
    }

    #[test]
    fn check_short_literal_string_escape_seq() {
        assert_eq!(
            short_literal_string::<VerboseError<&str>>(r"'\a'"),
            Ok(("", ShortLiteralString("\\a".into())))
        );
        assert!(short_literal_string::<VerboseError<&str>>(r"'\b'").is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r"'\f'").is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r"'\n'").is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r"'\r'").is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r"'\t'").is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r#""\v""#).is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r#""\\""#).is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r#"'\"'"#).is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r"'\''").is_ok());
        assert!(short_literal_string::<VerboseError<&str>>(r#"'\a""#).is_err()); // different quotes
        assert!(short_literal_string::<VerboseError<&str>>(r"'\z'").is_err());
    }

    #[test]
    fn check_short_literal_string_escape_seq_byte() {
        assert_eq!(
            short_literal_string::<VerboseError<&str>>(r"'\xAF'"),
            Ok(("", ShortLiteralString("\\xAF".into())))
        );
    }

    #[test]
    fn check_short_literal_string_escape_seq_digits() {
        assert_eq!(
            short_literal_string::<VerboseError<&str>>(r"'\123'"),
            Ok(("", ShortLiteralString("\\123".into())))
        );
        assert_eq!(
            short_literal_string::<VerboseError<&str>>(r"'\12'"),
            Ok(("", ShortLiteralString("\\12".into())))
        );
        assert_eq!(
            short_literal_string::<VerboseError<&str>>(r"'\1'"),
            Ok(("", ShortLiteralString("\\1".into())))
        );
    }

    #[test]
    fn check_short_literal_string_escape_seq_utf8() {
        assert_eq!(
            short_literal_string::<VerboseError<&str>>(r"'\uaF2'"),
            Ok(("", ShortLiteralString("\\uaF2".into())))
        );
    }
}
