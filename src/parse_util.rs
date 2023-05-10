#![deny(warnings, clippy::all, clippy::pedantic)]

use nom::{
    IResult,
    bytes::complete::take_while,
    sequence::preceded,
    combinator::eof,
};

pub fn parse_eof(i: &str) -> IResult<&str, &str> {
    preceded(parse_space, eof)(i)
}

pub fn parse_space(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}


