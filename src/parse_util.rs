use nom::{
    IResult,
    bytes::complete::{
        tag,
        take_while,
    },
    character::complete::{alpha1, alphanumeric1},
    sequence::{
        preceded,
        pair,
    },
    combinator::{
        eof,
        recognize,
    },
    branch::alt,
    multi::many0_count,
    error::VerboseError,
};

pub type ParseResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

pub fn parse_eof(i: &str) -> ParseResult<&str> {
    preceded(parse_space, eof)(i)
}

pub fn parse_space(i: &str) -> ParseResult<&str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

pub fn parse_identifier(i: &str) -> ParseResult<&str> {
  recognize(
    pair(
      alt((alpha1, tag("_"))),
      many0_count(alt((alphanumeric1, tag("_"))))
    )
  )(i)
}

