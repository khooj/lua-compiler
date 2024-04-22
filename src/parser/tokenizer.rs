use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, char, digit1, line_ending, multispace0, not_line_ending, one_of,
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
}
