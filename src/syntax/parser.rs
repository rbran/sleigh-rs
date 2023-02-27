use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::sequence::{delimited, pair, separated_pair, tuple};
use nom::IResult;

use crate::preprocessor::token::{Token, TokenType};
use crate::preprocessor::FilePreProcessor;
use crate::{
    Number, NumberNonZeroUnsigned, NumberUnsigned, RangeBits, SleighError, Span,
};

pub trait TokenHelper {
    fn location(&self) -> Span;
    fn expect(&self, value: TokenType) -> Result<(), SleighError>;
    fn ident(&self) -> Result<&str, SleighError>;
    fn string(&self) -> Result<&str, SleighError>;
    fn number(&self) -> Result<NumberUnsigned, SleighError>;
}

pub trait Parser {
    fn parse_until(
        &mut self,
        token_stream: &mut Vec<Token>,
        find: fn(&Token) -> bool,
    ) -> Result<(), SleighError>;
    fn token(&mut self) -> Result<Token, SleighError>;
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
impl From<SleighError> for nom::Err<SleighError> {
    fn from(value: SleighError) -> Self {
        nom::Err::Error(value)
    }
}
impl From<nom::Err<SleighError>> for SleighError {
    fn from(value: nom::Err<SleighError>) -> Self {
        match value {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(e) | nom::Err::Failure(e) => e,
        }
    }
}
impl nom::error::FromExternalError<&[Token], SleighError> for SleighError {
    fn from_external_error(
        _input: &[Token],
        _kind: nom::error::ErrorKind,
        e: SleighError,
    ) -> Self {
        e
    }
}

impl TokenHelper for Token {
    fn location(&self) -> Span {
        self.location.clone()
    }
    fn expect(&self, value: TokenType) -> Result<(), SleighError> {
        (self.token_type == value)
            .then_some(())
            .ok_or_else(|| SleighError::StatementInvalid(self.location()))
    }
    fn ident(&self) -> Result<&str, SleighError> {
        match &self.token_type {
            TokenType::Ident(value) => Ok(value),
            _ => Err(SleighError::StatementInvalid(self.location())),
        }
    }
    fn string(&self) -> Result<&str, SleighError> {
        match &self.token_type {
            TokenType::String(value) => Ok(value),
            _ => Err(SleighError::StatementInvalid(self.location())),
        }
    }
    fn number(&self) -> Result<NumberUnsigned, SleighError> {
        match &self.token_type {
            TokenType::Number(value) => Ok(*value),
            _ => Err(SleighError::StatementInvalid(self.location())),
        }
    }
}

impl Parser for FilePreProcessor {
    //TODO make this more nom ideomatic
    fn parse_until(
        &mut self,
        token_stream: &mut Vec<Token>,
        find: fn(&Token) -> bool,
    ) -> Result<(), SleighError> {
        while let Some(token) = self.parse()? {
            let found_end = find(&token);
            token_stream.push(token);
            if found_end {
                return Ok(());
            }
        }
        Err(SleighError::UnexpectedEof)
    }
    fn token(&mut self) -> Result<Token, SleighError> {
        self.parse()?.ok_or(SleighError::UnexpectedEof)
    }
}

pub fn this_ident<'a, 'b>(
    value: &'b str,
) -> impl FnMut(&'a [Token]) -> IResult<&'a [Token], &'a Span, SleighError> + 'b
{
    move |input: &'a [Token]| {
        let (first, rest) =
            input.split_first().ok_or(SleighError::UnexpectedEof)?;
        if first.ident()? != value {
            return Err(nom::Err::Error(SleighError::StatementInvalid(
                first.location.clone(),
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
    ("int2float") =>   { crate::preprocessor::token::TokenType::OpInt2float };
    ("float2float") => { crate::preprocessor::token::TokenType::OpFloat2float };
}

#[rustfmt::skip]
macro_rules! tag {
    ($x:tt) => { crate::syntax::parser::tag_token_type(token_type!($x)) };
}

pub fn tag_token_type(
    value: TokenType,
) -> impl FnMut(&[Token]) -> IResult<&[Token], &Span, SleighError> {
    move |input| {
        let (first, rest) =
            input.split_first().ok_or(SleighError::UnexpectedEof)?;
        if first.token_type != value {
            return Err(nom::Err::Error(SleighError::StatementInvalid(
                first.location.clone(),
            )));
        }
        Ok((rest, &first.location))
    }
}

pub fn option<'a, O, F>(
    mut parser: F,
) -> impl FnMut(
    &'a [Token],
) -> IResult<&'a [Token], (Option<O>, &'a Span), SleighError>
where
    F: FnMut(&'a [Token]) -> IResult<&'a [Token], (O, &'a Span), SleighError>,
{
    move |input| {
        alt((
            map(tag!("_"), |span| (None, span)),
            map(|x| parser(x), |(value, span)| (Some(value), span)),
        ))(input)
    }
}

pub fn ident_ref(
    input: &[Token],
) -> IResult<&[Token], (&str, &Span), SleighError> {
    let (first, rest) =
        input.split_first().ok_or(SleighError::UnexpectedEof)?;
    let name = first.ident()?;
    Ok((rest, (name, &first.location)))
}
pub fn ident(
    input: &[Token],
) -> IResult<&[Token], (String, &Span), SleighError> {
    let (rest, (ident, span)) = ident_ref(input)?;
    Ok((rest, (ident.to_owned(), span)))
}

pub fn string(
    input: &[Token],
) -> IResult<&[Token], (String, &Span), SleighError> {
    let (first, rest) =
        input.split_first().ok_or(SleighError::UnexpectedEof)?;
    let name = first.string()?;
    Ok((rest, (name.to_owned(), &first.location)))
}

pub fn number(
    input: &[Token],
) -> IResult<&[Token], (NumberUnsigned, &Span), SleighError> {
    let (first, rest) =
        input.split_first().ok_or(SleighError::UnexpectedEof)?;
    let number = first.number()?;
    Ok((rest, (number, &first.location)))
}
pub fn number_unsigned(
    input: &[Token],
) -> IResult<&[Token], (Number, &Span), SleighError> {
    map(number, |(number, span)| (Number::Positive(number), span))(input)
}
pub fn number_signed(
    input: &[Token],
) -> IResult<&[Token], (Number, &Span), SleighError> {
    let (rest, (is_neg, (number, location))) =
        pair(opt(tag!("-")), number)(input)?;
    let number = if is_neg.is_some() {
        Number::Negative(number)
    } else {
        Number::Positive(number)
    };
    Ok((rest, (number, location)))
}

pub fn parse_range_inclusive(
    input: &[Token],
) -> IResult<&[Token], RangeBits, SleighError> {
    let (rest, (_start, ((lsb_bit, _), (msb_bit, msb_bit_span)), _end)) =
        tuple((
            tag!("("),
            separated_pair(number, tag!(","), number),
            tag!(")"),
        ))(input)?;
    if msb_bit < lsb_bit {
        return Err(nom::Err::Error(SleighError::InvalidBitrange(
            msb_bit_span.clone(),
        )));
    }
    let n_bits = NumberNonZeroUnsigned::new((msb_bit + 1) - lsb_bit)
        .ok_or_else(|| SleighError::InvalidBitrange(msb_bit_span.clone()))?;
    let bitrange = RangeBits { lsb_bit, n_bits };
    Ok((rest, bitrange))
}
pub fn parse_lsb_len(
    input: &[Token],
) -> IResult<&[Token], RangeBits, SleighError> {
    let (rest, (_start, ((lsb_bit, _), (n_bits, n_bits_span)), _end)) =
        tuple((
            tag!("["),
            separated_pair(number, tag!(","), number),
            tag!("]"),
        ))(input)?;
    let n_bits = NumberNonZeroUnsigned::new(n_bits)
        .ok_or_else(|| SleighError::InvalidBitrange(n_bits_span.clone()))?;
    let bitrange = RangeBits { lsb_bit, n_bits };
    Ok((rest, bitrange))
}

//pub fn context_attributes(
//    input: &[Token],
//) -> IResult<&[Token], (bool, PrintFlags), SleighError> {
//    let mut rest = input;
//    let mut noflow_set = false;
//    let mut signed_set = false;
//    let mut base = None;
//    loop {
//        let token = rest.get(0).ok_or(SleighError::UnexpectedEof)?;
//        let att = match token {
//            Token {
//                location: _,
//                token_type: TokenType::Ident(x),
//            } => x,
//            Token {
//                location: _,
//                token_type: TokenType::StatementEnd,
//            } => break,
//            Token { location, .. } => {
//                return Err(nom::Err::Failure(SleighError::StatementInvalid(
//                    location.clone(),
//                )))
//            }
//        };
//        match att.as_str() {
//            "nowflow" if !noflow_set => noflow_set = true,
//            "nowflow" if noflow_set => {
//                return Err(nom::Err::Failure(SleighError::ContextAttDup(
//                    token.location.clone(),
//                )))
//            }
//            "signed" if !signed_set => signed_set = true,
//            "signed" if signed_set => {
//                return Err(nom::Err::Failure(SleighError::ContextAttDup(
//                    token.location.clone(),
//                )))
//            }
//            "hex" if base.is_none() => base = Some(PrintBase::Hex),
//            "dec" if base.is_none() => base = Some(PrintBase::Dec),
//            "hex" | "dec" if base.is_some() => {
//                return Err(nom::Err::Failure(SleighError::ContextAttDup(
//                    token.location.clone(),
//                )))
//            }
//            //not a att, must be the next token to end of statement
//            _ => break,
//        }
//        rest = &rest[1..];
//    }
//    Ok((rest, (noflow_set, PrintFlags { signed_set, base })))
//}

//pub fn token_field_attributes(
//    input: &[Token],
//) -> IResult<&[Token], PrintFlags, SleighError> {
//    let mut rest = input;
//    let mut signed_set = false;
//    let mut base = None;
//    loop {
//        let token = rest.get(0).ok_or(SleighError::UnexpectedEof)?;
//        let att = match token {
//            Token {
//                location: _,
//                token_type: TokenType::Ident(x),
//            } => x,
//            Token {
//                location: _,
//                token_type: TokenType::StatementEnd,
//            } => break,
//            Token { location, .. } => {
//                return Err(nom::Err::Failure(SleighError::StatementInvalid(
//                    location.clone(),
//                )))
//            }
//        };
//        match att.as_str() {
//            "signed" if !signed_set => signed_set = true,
//            "signed" if signed_set => {
//                return Err(nom::Err::Failure(SleighError::TokenFieldAttDup(
//                    token.location.clone(),
//                )))
//            }
//            "hex" if base.is_none() => base = Some(PrintBase::Hex),
//            "dec" if base.is_none() => base = Some(PrintBase::Dec),
//            "hex" | "dec" if base.is_some() => {
//                return Err(nom::Err::Failure(SleighError::TokenFieldAttDup(
//                    token.location.clone(),
//                )))
//            }
//            //not a att, must be the next token to end of statement
//            _ => break,
//        }
//        rest = &rest[1..];
//    }
//    Ok((rest, PrintFlags { signed_set, base }))
//}

//matches both X and "X"
fn string_inline(
    input: &[Token],
) -> IResult<&[Token], (String, &Span), SleighError> {
    alt((string, ident))(input)
}

pub fn fieldlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(String, Span)>, SleighError> {
    alt((
        map(ident, |(x, span)| vec![(x, span.clone())]),
        delimited(
            tag!("["),
            many1(map(ident, |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}

pub fn registerlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(Option<String>, Span)>, SleighError> {
    alt((
        map(ident, |(x, span)| vec![(Some(x), span.clone())]),
        delimited(
            tag!("["),
            many1(map(option(ident), |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}

pub fn numberlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(Option<Number>, Span)>, SleighError> {
    alt((
        map(number_signed, |(x, span)| vec![(Some(x), span.clone())]),
        delimited(
            tag!("["),
            many1(map(option(number_signed), |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}
pub fn stringlist(
    input: &[Token],
) -> IResult<&[Token], Vec<(Option<String>, Span)>, SleighError> {
    alt((
        map(string_inline, |(x, span)| vec![(Some(x), span.clone())]),
        delimited(
            tag!("["),
            many1(map(option(string_inline), |(x, span)| (x, span.clone()))),
            tag!("]"),
        ),
    ))(input)
}
