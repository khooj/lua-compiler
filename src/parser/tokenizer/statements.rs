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
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

use super::{
    expressions::{expression, Expression},
    variable::{variable, Variable},
};

#[derive(Debug)]
pub(crate) struct Statement;

pub(crate) fn statement<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Statement, E> {
    Ok((i, Statement))
}

#[derive(Debug)]
pub(crate) struct Block {
    statements: Vec<Statement>,
}

pub(crate) fn block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Block, E> {
    let (i, statements) = context("block", many1(statement))(i)?;
    Ok((i, Block { statements }))
}

#[derive(Debug)]
pub(crate) struct Chunk(Block);

#[derive(Debug)]
pub(crate) struct Assignment {
    varlist: Vec<Variable>,
    explist: Vec<Expression>,
}

pub(crate) fn assignment<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Assignment, E> {
    let (i, varlist) = many1(terminated(variable, opt(tag(", "))))(i)?;
    let (i, _) = tag(" = ")(i)?;
    let (i, explist) = many1(terminated(expression, opt(tag(", "))))(i)?;
    Ok((i, Assignment { varlist, explist }))
}

#[derive(Debug)]
pub(crate) struct If {
    cond: Expression,
    first_block: Block,
    elseifs: Vec<(Expression, Block)>,
    else_block: Option<Block>,
}

pub(crate) fn if_control_flow<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, If, E> {
    let (i, cond) = preceded(tag("if "), expression)(i)?;
    let (i, first_block) = preceded(tag(" then "), block)(i)?;
    let (i, elseifs) = many0(tuple((
        preceded(tag("elseif "), expression),
        preceded(tag(" then "), block),
    )))(i)?;
    let (i, else_block) = opt(preceded(tag("else "), block))(i)?;
    let (i, _) = tag("end")(i)?;
    Ok((
        i,
        If {
            cond,
            first_block,
            elseifs,
            else_block,
        },
    ))
}

#[cfg(test)]
mod tests {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn check_assignment() {
        assert!(assignment::<VerboseError<&str>>("x, y, z = 1, 2, 3").is_ok());
        assert!(assignment::<VerboseError<&str>>("i, a[i] = i+1, 20").is_ok());
    }
}
