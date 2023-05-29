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
};

pub fn parse_eof(i: &str) -> IResult<&str, &str> {
    preceded(parse_space, eof)(i)
}

pub fn parse_space(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

pub fn parse_identifier(i: &str) -> IResult<&str, &str> {
  recognize(
    pair(
      alt((alpha1, tag("_"))),
      many0_count(alt((alphanumeric1, tag("_"))))
    )
  )(i)
}

