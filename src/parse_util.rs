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
    Err,
    error::VerboseError,
};

pub trait ParseResultWrapper<'a, O> {
   fn wrap_failure(self) -> Result<Result<(&'a str, O), VerboseError<&'a str>>, nom::Err<VerboseError<&'a str>>>;
   fn unwrap_result(self) -> Result<(&'a str, O), VerboseError<&'a str>>;
}

impl<'a, O> ParseResultWrapper<'a, O> for ParseResult<'a, O> {
   fn wrap_failure(self) -> Result<Result<(&'a str, O), VerboseError<&'a str>>, nom::Err<VerboseError<&'a str>>> {
       match self {
           Ok(r) => Ok(Ok(r)),
           Err(Err::Error(err)) => Ok(Err(err)),
           Err(e) => Err(e),
       }
   }

   fn unwrap_result(self) -> Result<(&'a str, O), VerboseError<&'a str>> {
       match self {
           Ok(r) => Ok(r),
           Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(e),
           Err(Err::Incomplete(_)) => unreachable!(),
       }
   }
}

pub type ParseResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

pub fn parse_list<'a, T, U>(
    mut parse_separator: impl FnMut(&'a str) -> ParseResult<'a, U>,
    mut parse_element: impl FnMut(&'a str) -> ParseResult<'a, T>,
) -> impl FnMut(&'a str) -> ParseResult<'a, Vec<T>> {
    move |i: &'a str| {
        let mut elements: Vec<T> = Vec::new();
        let mut next_i = i;
        loop {
            let i = next_i;
            let Ok((i, element)) = parse_element(i).wrap_failure()? else {
                break
            };
            next_i = i;
            elements.push(element);

            let Ok((i, _)) = parse_separator(i).wrap_failure()? else {
                break
            };
            next_i = i
        };
        Ok((next_i, elements))
    }
}

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

