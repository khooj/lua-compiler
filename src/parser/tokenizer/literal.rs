use std::ops::Index;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while_m_n},
    character::{
        complete::{
            alpha1, alphanumeric0, alphanumeric1, char, digit1, hex_digit1, line_ending,
            multispace0, newline, not_line_ending, one_of,
        },
        is_alphabetic,
    },
    combinator::{cut, map, map_res, not, opt},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{count, length_count, many0, many0_count, many1, many_m_n, separated_list0},
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
pub(crate) struct Identifier {
    s: String,
}

enum Token {
    Identifier(Identifier),
}

pub(crate) fn identifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
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

fn literal_string_escape_seqs<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, String, E> {
    let escape_seq = tuple((tag("\\"), one_of("abfnrtv\\\"\'")));
    let hex_byte = tuple((tag("\\x"), take_while_m_n(2, 2, |c: char| c.is_digit(16))));
    let digits = tuple((tag("\\"), take_while_m_n(1, 3, |c: char| c.is_digit(10))));
    let utf_char = tuple((tag("\\u"), take_while_m_n(3, 3, |c: char| c.is_digit(16))));
    let (i, res) = context(
        "string_literal_escape_seq",
        alt((
            map(escape_seq, |(c, c2)| (c, c2.to_string())),
            map(hex_byte, |(c, c2): (&str, &str)| (c, c2.to_string())),
            map(digits, |(c, c2): (&str, &str)| (c, c2.to_string())),
            map(utf_char, |(c, c2): (&str, &str)| (c, c2.to_string())),
        )),
    )(i)?;
    Ok((i, format!("{}{}", res.0, res.1)))
}

fn literal_string_content<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, String, E> {
    let (i, res) = context(
        "literal_string_content",
        many0(alt((
            literal_string_escape_seqs,
            map(alphanumeric1, |s: &str| s.to_string()),
        ))),
    )(i)?;
    Ok((i, res.into_iter().collect::<String>()))
}

#[derive(Debug, PartialEq)]
struct LiteralString(String);

fn literal_string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, LiteralString, E> {
    let single = context(
        "literal_string_single",
        delimited(char('\''), literal_string_content, char('\'')),
    );
    let double = context(
        "literal_string_double",
        delimited(char('\"'), literal_string_content, char('\"')),
    );
    let (i, lit) = context("literal_string", alt((single, double)))(i)?;
    Ok((i, LiteralString(lit)))
}

fn multiline_literal_string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, LiteralString, E> {
    let (i, _) = context("opening_bracket", char('['))(i)?;
    let (i, eq_count) = many0_count(char('='))(i)?;
    let (i, _) = context("opening_bracket_end", char('['))(i)?;
    let (i, _) = context("multiline_newline", opt(line_ending))(i)?;
    let (i, content) = context(
        "multiline_content",
        take_while(|c: char| {
            c != ']'
                && (c.is_ascii_alphanumeric()
                    || c.is_ascii_whitespace()
                    || c.is_ascii_punctuation())
        }),
    )(i)?;
    let (i, _) = context(
        "multiline_closing_bracket",
        tuple((
            char(']'),
            many_m_n(eq_count, eq_count, char('=')),
            char(']'),
        )),
    )(i)?;
    Ok((i, LiteralString(content.to_string())))
}

fn integer_numeric_constant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, String, E> {
    let hex = context(
        "integer_numeric_constant hex",
        preceded(alt((tag("0x"), tag("0X"))), hex_digit1),
    );
    let int = context("integer_numeric_constant int", digit1);
    let (i, s) = alt((hex, int))(i)?;
    Ok((i, s.to_string()))
}

fn float_numeric_constant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, String, E> {
    let hex = context(
        "float_numeric_constant hex",
        take_while(|c: char| {
            c.is_ascii_hexdigit() || ['x', 'X', '.', 'e', 'E', 'p', 'P', '-', '+'].contains(&c)
        }),
    );
    let float = context(
        "float_numeric_constant float",
        take_while(|c: char| c.is_ascii_digit() || ['.', 'e', 'E', '-'].contains(&c)),
    );
    let (i, s) = alt((hex, float))(i)?;
    Ok((i, s.to_string()))
}

#[derive(PartialEq, Debug)]
struct NumericConstant(String);

fn numeric_constant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, NumericConstant, E> {
    let (i, s) = context(
        "numeric_constant",
        alt((float_numeric_constant, integer_numeric_constant)),
    )(i)?;
    Ok((i, NumericConstant(s)))
}

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
    fn check_literal_string_content() {
        match literal_string_content::<VerboseError<&str>>(r"\a") {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("err: {}", convert_error(r"\a", e));
                assert!(false);
            }
            Ok((_, s)) => assert_eq!(s, "\\a"),
            _ => assert!(false),
        };
        match literal_string_content::<VerboseError<&str>>(r"asd") {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("err: {}", convert_error(r"asd", e));
                assert!(false);
            }
            Ok((_, s)) => assert_eq!(s, "asd"),
            _ => assert!(false),
        };
    }

    #[test]
    fn check_literal_string_escape_seqs_content() {
        match literal_string_escape_seqs::<VerboseError<&str>>(r"\a") {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("err: {}", convert_error(r"\a", e));
                assert!(false);
            }
            Ok((_, s)) => assert_eq!(s, "\\a"),
            _ => assert!(false),
        };
    }

    #[test]
    fn check_short_literal_string_escape_seq() {
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\b").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\f").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\n").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\r").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\t").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\v").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\\").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>("\\\"").is_ok());
        assert!(literal_string_escape_seqs::<VerboseError<&str>>(r"\'").is_ok());
    }

    #[test]
    fn check_short_literal_string_escape_seq_byte() {
        let input = r"\xAF";
        let res = literal_string_escape_seqs::<VerboseError<&str>>(input);
        match res {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("err: {}", convert_error(input, e));
                assert!(false);
            }
            _ => {}
        };
    }

    #[test]
    fn check_short_literal_string_escape_seq_digits() {
        assert_eq!(
            literal_string_escape_seqs::<VerboseError<&str>>(r"\123"),
            Ok(("", "\\123".into()))
        );
        assert_eq!(
            literal_string_escape_seqs::<VerboseError<&str>>(r"\12"),
            Ok(("", "\\12".into()))
        );
        assert_eq!(
            literal_string_escape_seqs::<VerboseError<&str>>(r"\1"),
            Ok(("", "\\1".into()))
        );
    }

    #[test]
    fn check_short_literal_string_escape_seq_utf8() {
        assert_eq!(
            literal_string_escape_seqs::<VerboseError<&str>>(r"\uaF2"),
            Ok(("", "\\uaF2".into()))
        );
    }

    #[test]
    fn check_multiline_literal_string() {
        let input = r#"[[alo
123"]]"#;
        match multiline_literal_string::<VerboseError<&str>>(input) {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("err: {}", convert_error(input, e));
                assert!(false);
            }
            Ok((_, s)) => assert_eq!(s.0, "alo\n123\""),
            _ => {}
        };
        let input = r#"[==[alo
123"]==]"#;
        match multiline_literal_string::<VerboseError<&str>>(input) {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("err: {}", convert_error(input, e));
                assert!(false);
            }
            Ok((_, s)) => assert_eq!(s.0, "alo\n123\""),
            _ => {}
        };
    }

    #[test]
    fn check_multiline_literal_string_error() {
        let input = r#"[=[alo
123"]]"#;
        match multiline_literal_string::<VerboseError<&str>>(input) {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {}
            Ok((_, s)) => assert!(false, "{}", s.0.as_str()),
            _ => {}
        };
        let input = r#"[=[alo
123"]==]"#;
        match multiline_literal_string::<VerboseError<&str>>(input) {
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {}
            Ok((_, s)) => assert!(false, "{}", s.0.as_str()),
            _ => {}
        };
    }

    #[test]
    fn check_numeric_constant() {
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("345"),
            Ok(("", NumericConstant("345".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("0xff"),
            Ok(("", NumericConstant("0xff".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("0XBEBADA"),
            Ok(("", NumericConstant("0XBEBADA".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("3.0"),
            Ok(("", NumericConstant("3.0".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("3.16e-2"),
            Ok(("", NumericConstant("3.16e-2".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("316e2"),
            Ok(("", NumericConstant("316e2".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("0x0.1E"),
            Ok(("", NumericConstant("0x0.1E".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("0xA23p-4"),
            Ok(("", NumericConstant("0xA23p-4".into())))
        );
        assert_eq!(
            numeric_constant::<VerboseError<&str>>("0x1.A23P+4"),
            Ok(("", NumericConstant("0x1.A23P+4".into())))
        );
    }
}
