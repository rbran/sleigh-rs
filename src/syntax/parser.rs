use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::sequence::{delimited, pair};
use nom::IResult;

use crate::preprocessor::token::{Token, TokenType};
use crate::preprocessor::FilePreProcessor;
use crate::{Number, NumberUnsigned, SleighError, Span};

pub trait TokenHelper {
    fn location(&self) -> Span;
    fn ident(&self) -> Result<&str, Box<SleighError>>;
    fn string(&self) -> Result<&str, Box<SleighError>>;
    fn number(&self) -> Result<NumberUnsigned, Box<SleighError>>;
}

pub trait Parser {
    fn parse_until(
        &mut self,
        token_stream: &mut Vec<Token>,
        find: fn(&Token) -> bool,
    ) -> Result<(), Box<SleighError>>;
}

impl<'a> nom::error::ParseError<&'a [Token]> for SleighError {
    fn from_error_kind(
        input: &'a [Token],
        kind: nom::error::ErrorKind,
    ) -> Self {
        if let Some(first) = input.first() {
            Self::UnableToParse(first.location.clone(), kind)
        } else {
            Self::UnexpectedEof
        }
    }

    fn append(
        _input: &'a [Token],
        _kind: nom::error::ErrorKind,
        other: Self,
    ) -> Self {
        other
    }
}
impl From<Box<SleighError>> for nom::Err<Box<SleighError>> {
    fn from(value: Box<SleighError>) -> Self {
        nom::Err::Error(value)
    }
}
impl From<nom::Err<Box<SleighError>>> for Box<SleighError> {
    fn from(value: nom::Err<Box<SleighError>>) -> Self {
        match value {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(e) | nom::Err::Failure(e) => e,
        }
    }
}
impl nom::error::FromExternalError<&[Token], Box<SleighError>>
    for Box<SleighError>
{
    fn from_external_error(
        _input: &[Token],
        _kind: nom::error::ErrorKind,
        e: Box<SleighError>,
    ) -> Self {
        e
    }
}

impl TokenHelper for Token {
    fn location(&self) -> Span {
        self.location.clone()
    }
    fn ident(&self) -> Result<&str, Box<SleighError>> {
        match &self.token_type {
            TokenType::Ident(value) => Ok(value),
            _ => Err(Box::new(SleighError::StatementInvalid(self.location()))),
        }
    }
    fn string(&self) -> Result<&str, Box<SleighError>> {
        match &self.token_type {
            TokenType::String(value) => Ok(value),
            _ => Err(Box::new(SleighError::StatementInvalid(self.location()))),
        }
    }
    fn number(&self) -> Result<NumberUnsigned, Box<SleighError>> {
        match &self.token_type {
            TokenType::Number(value) => Ok(*value),
            _ => Err(Box::new(SleighError::StatementInvalid(self.location()))),
        }
    }
}

impl Parser for FilePreProcessor {
    //TODO make this more nom ideomatic
    fn parse_until(
        &mut self,
        token_stream: &mut Vec<Token>,
        find: fn(&Token) -> bool,
    ) -> Result<(), Box<SleighError>> {
        while let Some(token) = self.parse()? {
            let found_end = find(&token);
            token_stream.push(token);
            if found_end {
                return Ok(());
            }
        }
        Err(Box::new(SleighError::UnexpectedEof))
    }
}

#[allow(clippy::type_complexity)]
pub fn this_ident<'a, 'b>(
    value: &'b str,
) -> impl FnMut(&'a [Token]) -> IResult<&'a [Token], &'a Span, Box<SleighError>> + 'b
{
    move |input: &'a [Token]| {
        let (first, rest) = input
            .split_first()
            .ok_or_else(|| Box::new(SleighError::UnexpectedEof))?;
        if first.ident()? != value {
            return Err(nom::Err::Error(Box::new(
                SleighError::StatementInvalid(first.location.clone()),
            )));
        }
        Ok((rest, &first.location))
    }
}
#[rustfmt::skip]
macro_rules! token_type {
    ("$and") =>        { crate::preprocessor::token::TokenType::OpBoolAnd };
    ("$xor") =>        { crate::preprocessor::token::TokenType::OpBoolXor };
    ("$or") =>         { crate::preprocessor::token::TokenType::OpBoolOr };
    ("f!=") =>         { crate::preprocessor::token::TokenType::OpFnotequal };
    ("f<=") =>         { crate::preprocessor::token::TokenType::OpFlessequal };
    ("f==") =>         { crate::preprocessor::token::TokenType::OpFequal };
    ("f>=") =>         { crate::preprocessor::token::TokenType::OpFgreatequal };
    ("f*") =>          { crate::preprocessor::token::TokenType::OpFmult };
    ("f+") =>          { crate::preprocessor::token::TokenType::OpFadd };
    ("f-") =>          { crate::preprocessor::token::TokenType::OpFsub };
    ("f/") =>          { crate::preprocessor::token::TokenType::OpFdiv };
    ("f<") =>          { crate::preprocessor::token::TokenType::OpFless };
    ("f>") =>          { crate::preprocessor::token::TokenType::OpFgreat };
    ("s<=") =>         { crate::preprocessor::token::TokenType::OpSlessequal };
    ("s>=") =>         { crate::preprocessor::token::TokenType::OpSgreatequal };
    ("s>>") =>         { crate::preprocessor::token::TokenType::OpSright };
    ("s%") =>          { crate::preprocessor::token::TokenType::OpSrem };
    ("s/") =>          { crate::preprocessor::token::TokenType::OpSdiv };
    ("s<") =>          { crate::preprocessor::token::TokenType::OpSless };
    ("s>") =>          { crate::preprocessor::token::TokenType::OpSgreat };
    ("!=") =>          { crate::preprocessor::token::TokenType::OpNotequal };
    ("<<") =>          { crate::preprocessor::token::TokenType::OpLeft };
    ("<=") =>          { crate::preprocessor::token::TokenType::OpLessequal };
    ("==") =>          { crate::preprocessor::token::TokenType::OpEqual };
    (">=") =>          { crate::preprocessor::token::TokenType::OpGreatequal };
    (">>") =>          { crate::preprocessor::token::TokenType::OpRight };
    ("&&") =>          { crate::preprocessor::token::TokenType::OpBoolAnd };
    ("^^") =>          { crate::preprocessor::token::TokenType::OpBoolXor };
    ("||") =>          { crate::preprocessor::token::TokenType::OpBoolOr };
    (">") =>           { crate::preprocessor::token::TokenType::OpGreat };
    ("<") =>           { crate::preprocessor::token::TokenType::OpLess };
    ("^") =>           { crate::preprocessor::token::TokenType::OpXor };
    ("&") =>           { crate::preprocessor::token::TokenType::OpAnd };
    ("|") =>           { crate::preprocessor::token::TokenType::OpOr };
    ("~") =>           { crate::preprocessor::token::TokenType::OpNeg };
    ("!") =>           { crate::preprocessor::token::TokenType::OpBitNeg };
    ("+") =>           { crate::preprocessor::token::TokenType::OpAdd };
    ("-") =>           { crate::preprocessor::token::TokenType::OpSub };
    ("*") =>           { crate::preprocessor::token::TokenType::OpMul };
    ("/") =>           { crate::preprocessor::token::TokenType::OpDiv };
    ("%") =>           { crate::preprocessor::token::TokenType::OpRem };
    ("=") =>           { crate::preprocessor::token::TokenType::OpAssign };
    ("(") =>           { crate::preprocessor::token::TokenType::DeliOpenParenteses };
    (")") =>           { crate::preprocessor::token::TokenType::DeliCloseParenteses };
    ("[") =>           { crate::preprocessor::token::TokenType::DeliOpenBrackets };
    ("]") =>           { crate::preprocessor::token::TokenType::DeliCloseBrackets };
    ("{") =>           { crate::preprocessor::token::TokenType::DeliOpenCurly };
    ("}") =>           { crate::preprocessor::token::TokenType::DeliCloseCurly };
    (",") =>           { crate::preprocessor::token::TokenType::Comma };
    (":") =>           { crate::preprocessor::token::TokenType::DubleDot };
    (";") =>           { crate::preprocessor::token::TokenType::StatementEnd };
    ("_") =>           { crate::preprocessor::token::TokenType::Underline };
    ("if") =>          { crate::preprocessor::token::TokenType::IfKey };
    ("...") =>         { crate::preprocessor::token::TokenType::Ellipsis };
    ("abs") =>         { crate::preprocessor::token::TokenType::OpAbs };
    ("nan") =>         { crate::preprocessor::token::TokenType::OpNan };
    ("call") =>        { crate::preprocessor::token::TokenType::CallKey };
    ("ceil") =>        { crate::preprocessor::token::TokenType::OpCeil };
    ("goto") =>        { crate::preprocessor::token::TokenType::GotoKey };
    ("sqrt") =>        { crate::preprocessor::token::TokenType::OpFloatSqrt };
    ("sext") =>        { crate::preprocessor::token::TokenType::OpSext };
    ("zext") =>        { crate::preprocessor::token::TokenType::OpZext };
    ("const") =>       { crate::preprocessor::token::TokenType::SpaceConst };
    ("trunc") =>       { crate::preprocessor::token::TokenType::OpTrunc };
    ("carry") =>       { crate::preprocessor::token::TokenType::OpCarry };
    ("floor") =>       { crate::preprocessor::token::TokenType::OpFloor };
    ("local") =>       { crate::preprocessor::token::TokenType::LocalKey };
    ("round") =>       { crate::preprocessor::token::TokenType::OpRound };
    ("unimpl") =>      { crate::preprocessor::token::TokenType::Unimpl };
    ("unique") =>      { crate::preprocessor::token::TokenType::SpaceUnique };
    ("return") =>      { crate::preprocessor::token::TokenType::ReturnKey };
    ("scarry") =>      { crate::preprocessor::token::TokenType::OpScarry };
    ("borrow") =>      { crate::preprocessor::token::TokenType::OpBorrow };
    ("sborrow") =>     { crate::preprocessor::token::TokenType::OpSborrow };
    ("epsilon") =>     { crate::preprocessor::token::TokenType::Epsilon };
    ("popcount") =>    { crate::preprocessor::token::TokenType::OpPopcount };
    ("lzcount") =>    { crate::preprocessor::token::TokenType::OpLzcount };
    ("int2float") =>   { crate::preprocessor::token::TokenType::OpInt2float };
    ("float2float") => { crate::preprocessor::token::TokenType::OpFloat2float };
}

#[rustfmt::skip]
macro_rules! tag {
    ($x:tt) => { crate::syntax::parser::tag_token_type(token_type!($x)) };
}

#[allow(clippy::type_complexity)]
pub fn tag_token_type(
    value: TokenType,
) -> impl FnMut(&[Token]) -> IResult<&[Token], &Span, Box<SleighError>> {
    move |input| {
        let (first, rest) = input
            .split_first()
            .ok_or_else(|| Box::new(SleighError::UnexpectedEof))?;
        if first.token_type != value {
            return Err(nom::Err::Error(Box::new(
                SleighError::StatementInvalid(first.location.clone()),
            )));
        }
        Ok((rest, &first.location))
    }
}

#[allow(clippy::type_complexity)]
pub fn option<'a, O, F>(
    mut parser: F,
) -> impl FnMut(
    &'a [Token],
) -> IResult<&'a [Token], (Option<O>, &'a Span), Box<SleighError>>
where
    F: FnMut(
        &'a [Token],
    ) -> IResult<&'a [Token], (O, &'a Span), Box<SleighError>>,
{
    // can't delete parser call without trouble
    #[allow(clippy::redundant_closure)]
    move |input| {
        alt((
            map(tag!("_"), |span| (None, span)),
            map(|x| parser(x), |(value, span)| (Some(value), span)),
        ))(input)
    }
}

#[allow(clippy::type_complexity)]
pub fn ident_ref(
    input: &[Token],
) -> IResult<&[Token], (&str, &Span), Box<SleighError>> {
    let (first, rest) = input
        .split_first()
        .ok_or_else(|| Box::new(SleighError::UnexpectedEof))?;
    let name = first.ident()?;
    Ok((rest, (name, &first.location)))
}
#[allow(clippy::type_complexity)]
pub fn ident(
    input: &[Token],
) -> IResult<&[Token], (String, &Span), Box<SleighError>> {
    let (rest, (ident, span)) = ident_ref(input)?;
    Ok((rest, (ident.to_owned(), span)))
}

#[allow(clippy::type_complexity)]
pub fn string(
    input: &[Token],
) -> IResult<&[Token], (String, &Span), Box<SleighError>> {
    let (first, rest) = input
        .split_first()
        .ok_or_else(|| Box::new(SleighError::UnexpectedEof))?;
    let name = first.string()?;
    Ok((rest, (name.to_owned(), &first.location)))
}

#[allow(clippy::type_complexity)]
pub fn number(
    input: &[Token],
) -> IResult<&[Token], (NumberUnsigned, &Span), Box<SleighError>> {
    let (first, rest) = input
        .split_first()
        .ok_or_else(|| Box::new(SleighError::UnexpectedEof))?;
    let number = first.number()?;
    Ok((rest, (number, &first.location)))
}
#[allow(clippy::type_complexity)]
pub fn number_unsigned(
    input: &[Token],
) -> IResult<&[Token], (Number, &Span), Box<SleighError>> {
    map(number, |(number, span)| (Number::Positive(number), span))(input)
}
#[allow(clippy::type_complexity)]
pub fn number_signed(
    input: &[Token],
) -> IResult<&[Token], (Number, &Span), Box<SleighError>> {
    let (rest, (is_neg, (number, location))) =
        pair(opt(tag!("-")), number)(input)?;
    let number = if is_neg.is_some() {
        Number::Negative(number)
    } else {
        Number::Positive(number)
    };
    Ok((rest, (number, location)))
}

//matches both X and "X"
#[allow(clippy::type_complexity)]
fn string_inline(
    input: &[Token],
) -> IResult<&[Token], (String, &Span), Box<SleighError>> {
    alt((string, ident))(input)
}

#[allow(clippy::type_complexity)]
pub fn fieldlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(String, Span)>, Box<SleighError>> {
    alt((
        map(ident, |(x, span)| vec![(x, span.clone())]),
        delimited(
            tag!("["),
            many1(map(ident, |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}

#[allow(clippy::type_complexity)]
pub fn registerlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(Option<String>, Span)>, Box<SleighError>> {
    alt((
        map(ident, |(x, span)| vec![(Some(x), span.clone())]),
        delimited(
            tag!("["),
            many1(map(option(ident), |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}

#[allow(clippy::type_complexity)]
pub fn numberlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(Option<Number>, Span)>, Box<SleighError>> {
    alt((
        map(number_signed, |(x, span)| vec![(Some(x), span.clone())]),
        delimited(
            tag!("["),
            many1(map(option(number_signed), |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}
#[allow(clippy::type_complexity)]
pub fn stringlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(Option<String>, Span)>, Box<SleighError>> {
    alt((
        map(string_inline, |(x, span)| vec![(Some(x), span.clone())]),
        delimited(
            tag!("["),
            many1(map(option(string_inline), |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}
