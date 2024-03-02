pub mod ifs;
pub mod macros;
pub mod parser;
pub mod token;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space0};
use nom::combinator::{consumed, eof, map, value};
use nom::sequence::pair;
use nom::IResult;
use thiserror::Error;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use parser::empty_space0;

use crate::preprocessor::macros::MacroLine;
use crate::{FileLocation, FileSpan, Location, MacroLocation, Span};

use ifs::IfCheckOwned;
use macros::{expansion, DefineDataOwned};
use parser::display_token;
use token::{Token, TokenType};

const MACRO_IF: &str = "@if";
const MACRO_IFDEF: &str = "@ifdef";
const MACRO_IFNDEF: &str = "@ifndef";
const MACRO_ELIF: &str = "@elif";
const MACRO_ELSE: &str = "@else";
const MACRO_ENDIF: &str = "@endif";

const MACRO_DEFINE: &str = "@define";
const MACRO_UNDEFINE: &str = "@undef";
const MACRO_INCLUDE: &str = "@include";

#[derive(Error, Debug)]
pub enum PreprocessorError {
    #[error("IO Error {0}")]
    File(#[from] std::io::Error),
    #[error("Preprocessor Parsing error at\n{0}\n")]
    Parse(#[from] nom::Err<nom::error::Error<Span>>),
    #[error("Preprocessor Execute at {message} {src}")]
    Execute { message: &'static str, src: Span },
    #[error("Comparing undefined Value {0}\n")]
    ComparingUndefined(FileSpan),
    #[error("Comparing Empty Value {0}\n")]
    ComparingEmpty(FileSpan),
    #[error("Expanding Undefined Value {0}\n")]
    ExpandingUndefined(FileSpan),
    #[error("Expanding Empty Value {0}\n")]
    ExpandingEmpty(FileSpan),
    #[error("Alias undefined Value {0}\n")]
    AliasUndefined(FileSpan),
    #[error("Alias with empty Value {0}\n")]
    AliasEmpty(FileSpan),
    #[error("Deleting undefined Value {0}\n")]
    DeleteUndefined(FileSpan),
    //NOTE this is allowed, just overwride the value
    //#[error("Defined value already exists {0}\n")]
    //DuplicatedDefine(FileSpan),
    #[error("Unable to parse Token at {0}\n")]
    PreprocessorToken(Location),
    #[error("Block was never opened {0}\n")]
    InvalidEndIf(FileSpan),
    #[error("Display was not closed correctly\n")]
    UnclosedDisplay,
    #[error("Unable to find If-Block closing {0}\n")]
    NotFoundEndIf(FileSpan),
}

#[derive(Debug)]
pub struct Define {
    pub name: Rc<str>,
    pub value: Option<String>,
    pub macro_location: FileSpan,
    pub value_location: FileSpan,
}

#[derive(Debug, Copy, Clone)]
enum IfStatus {
    If,
    IfElse,
    Else,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IfBorders {
    If,
    IfDef,
    IfNDef,
    IfElse,
    Else,
    EndIf,
}

#[derive(Debug)]
enum DrainingSource {
    File(DrainingFile),
    Macro(DrainingMacro),
}
#[derive(Debug)]
pub(crate) struct DrainingMacro {
    location: MacroLocation,
    data: String,
    position: usize,
}
impl DrainingMacro {
    fn new(
        defined: &Define,
        location: FileSpan,
    ) -> Result<Self, Box<PreprocessorError>> {
        let Some(defined_value) = &defined.value else {
            return Err(Box::new(PreprocessorError::ExpandingEmpty(location)));
        };
        Ok(Self {
            location: MacroLocation {
                value: defined.value_location.clone(),
                expansion: location,
                line: 0,
                column: 0,
            },
            data: defined_value.clone(),
            position: 0,
        })
    }
    fn location(&self) -> &MacroLocation {
        &self.location
    }
    fn nom_it<'a, O, F, E>(
        &'a mut self,
        nom: F,
    ) -> Result<(O, Span), Box<PreprocessorError>>
    where
        O: 'a,
        F: FnMut(&'a str) -> IResult<&'a str, O, E>,
        E: nom::error::ParseError<&'a str>,
    {
        //extract the value
        let (_, (skip, value)) = consumed(nom)(&self.data[self.position..])
            .map_err(|_| {
                Box::new(PreprocessorError::PreprocessorToken(Location::Macro(
                    self.location().clone(),
                )))
            })?;
        //calculate the currect line/column after consumption
        let end_line = self.location.line + skip.split('\n').count() as u64;
        let end_column = skip
            .rsplit_once(&['\n', '\r'])
            .map(|x| x.1.chars().count() as u64)
            .unwrap_or(self.location.column + skip.chars().count() as u64);
        // return this token location
        let location = self.location().end_span(end_line, end_column);
        //update the currect drain location
        self.location.line = end_line;
        self.location.column = end_column;
        self.position += skip.len();
        //return values
        Ok((value, Span::Macro(location)))
    }
    fn parse_display(
        &mut self,
    ) -> Result<Option<DisplayToken>, Box<PreprocessorError>> {
        self.nom_it(display_token)
            .map(|(token, span)| match token? {
                parser::Display::End => Some(DisplayToken::End),
                parser::Display::Concat => Some(DisplayToken::Concat),
                parser::Display::Ident(ident) => {
                    Some(DisplayToken::Ident(span, ident))
                }
                parser::Display::Other(c) => Some(DisplayToken::Other(c)),
                parser::Display::Literal(lit) => {
                    Some(DisplayToken::Literal(lit))
                }
            })
    }
    fn parse(&mut self) -> Result<Option<Token>, Box<PreprocessorError>> {
        //eat all the empty spaces
        let _ = self.nom_it::<_, _, ()>(space0).unwrap();
        //parse a token or None if eof
        self.nom_it(alt((
            value(None, eof),
            map(TokenType::parse, Option::Some),
        )))
        .map(|(token_type, location)| Some(Token::new(location, token_type?)))
    }
}

#[derive(Clone, Debug)]
enum DrainingFileProduct {
    Token(Token),
    File(String, FileSpan),
    Expand(String, FileSpan),
    End,
}

#[derive(Clone, Debug)]
enum DrainingFileBody {
    Macro(MacroLine, FileSpan),
    Expansion(String, FileSpan),
    Token(Token),
    End,
}
#[derive(Debug)]
pub(crate) struct DrainingFile {
    location: FileLocation,
    if_stack: Vec<(FileSpan, IfStatus)>,
    data: String,
    position: usize,
}
impl DrainingFile {
    fn new(file: &Path) -> Result<Self, Box<PreprocessorError>> {
        let data =
            std::fs::read_to_string(file).map_err(|e| Box::new(e.into()))?;
        Ok(Self {
            data,
            position: 0,
            location: FileLocation {
                file: Rc::from(file),
                line: 0,
                column: 0,
            },
            if_stack: vec![],
        })
    }
    fn location(&self) -> &FileLocation {
        &self.location
    }
    fn data(&self) -> &str {
        &self.data[self.position..]
    }
    fn nom_it<'a, O, F, E>(
        &'a mut self,
        nom: F,
    ) -> Result<(O, FileSpan), Box<PreprocessorError>>
    where
        O: 'a,
        F: FnMut(&'a str) -> IResult<&'a str, O, E>,
        E: nom::error::ParseError<&'a str>,
    {
        //extract the value
        let (_, (skip, value)) = consumed(nom)(&self.data[self.position..])
            .map_err(|_| {
                Box::new(PreprocessorError::PreprocessorToken(Location::File(
                    self.location().clone(),
                )))
            })?;
        //calculate the currect line/column after consumption
        let end_line = self.location.line + skip.split('\n').count() as u64 - 1;
        let end_column = skip
            .rsplit_once(&['\n', '\r'])
            .map(|x| x.1.chars().count() as u64)
            .unwrap_or(self.location.column + skip.chars().count() as u64);
        // return this token location
        let location = self.location().end_span(end_line, end_column);
        //update the currect drain location
        self.location.line = end_line;
        self.location.column = end_column;
        self.position += skip.len();
        //return values
        Ok((value, location))
    }
    fn next_if_block<F>(
        &mut self,
        src: &FileSpan,
        mut f: F,
    ) -> Result<IfBorders, Box<PreprocessorError>>
    where
        F: FnMut(IfBorders) -> bool,
    {
        use IfBorders::*;
        let mut counter = 0u32;
        let data = &self.data[self.position..];
        let found = data
            .lines()
            .enumerate()
            .filter_map(|(line_num, line)| {
                //check this line is a macro
                let (_rest, found_macro) = alt::<_, _, (), _>((
                    value(If, pair(space0, tag(MACRO_IF))),
                    value(IfDef, pair(space0, tag(MACRO_IFDEF))),
                    value(IfNDef, pair(space0, tag(MACRO_IFNDEF))),
                    value(IfElse, pair(space0, tag(MACRO_ELIF))),
                    value(Else, pair(space0, tag(MACRO_ELSE))),
                    value(EndIf, pair(space0, tag(MACRO_ENDIF))),
                ))(line)
                .ok()?;
                Some((line_num, line, found_macro))
            })
            .find_map(|(line_num, line, found)| {
                match (counter, found) {
                    //ignore the sub if
                    (_, If | IfDef | IfNDef) => counter += 1,
                    //if in a sub if, just close it
                    (1.., EndIf) => counter -= 1,
                    //if sub-if-block ifElse or Else, just ignore it
                    (1.., IfElse | Else) => (),
                    //found the end of the block, don't consume macro, just exit
                    (0, block) if f(block) => {
                        return Some((line_num, line, block))
                    }
                    //found a block that we don't care about
                    (0, _block) => (),
                }
                None
            });
        let Some((line_num, line, block)) = found else {
            return Err(Box::new(PreprocessorError::NotFoundEndIf(
                src.clone(),
            )));
        };

        let skip = line.as_ptr() as usize - data.as_ptr() as usize;
        self.position += skip;
        self.location.line += line_num as u64;
        self.location.column = 0;
        Ok(block)
    }

    fn enter_if_block(
        &mut self,
        cond: bool,
        state: &PreProcessorState,
        src: &FileSpan,
    ) -> Result<(), Box<PreprocessorError>> {
        //if this if is true, just to inside it
        if cond {
            self.if_stack.push((src.clone(), IfStatus::If));
            return Ok(());
        }
        //othewise find the next block and check if we will enter it
        loop {
            let _next_block = self.next_if_block(src, |_| true)?;
            match self.nom_it(MacroLine::parse)? {
                (MacroLine::ElIf(cond), src) => {
                    if check_cond(state, cond, &src)? {
                        self.if_stack.push((src.clone(), IfStatus::IfElse));
                        break;
                    } else {
                        //not the block that we can enter, search the next
                        continue;
                    }
                }
                (MacroLine::Else, _) => {
                    self.if_stack.push((src.clone(), IfStatus::Else));
                    break;
                }
                (MacroLine::EndIf, _) => {
                    //we never entered any if-block
                    break;
                }
                (_, _) => unreachable!(),
            }
        }
        Ok(())
    }
    fn parse_macro_line(&mut self) -> Option<DrainingFileBody> {
        let (macro_line, location) = self.nom_it(MacroLine::parse).ok()?;
        Some(DrainingFileBody::Macro(macro_line, location))
    }
    fn parse_display(
        &mut self,
    ) -> Result<Option<DisplayToken>, Box<PreprocessorError>> {
        self.nom_it(display_token)
            .map(|(token, span)| match token? {
                parser::Display::End => Some(DisplayToken::End),
                parser::Display::Concat => Some(DisplayToken::Concat),
                parser::Display::Ident(ident) => {
                    Some(DisplayToken::Ident(Span::File(span), ident))
                }
                parser::Display::Other(c) => Some(DisplayToken::Other(c)),
                parser::Display::Literal(lit) => {
                    Some(DisplayToken::Literal(lit))
                }
            })
    }
    fn parse_body(
        &mut self,
    ) -> Result<DrainingFileBody, Box<PreprocessorError>> {
        //a macro need to be after a new line or the first thing in the file
        if self.position == 0 {
            if let Some(x) = self.parse_macro_line() {
                return Ok(x);
            }
        }
        loop {
            //first consume the empty space, but not the new line
            //never fails
            let _ = self.nom_it(empty_space0).unwrap();

            //then check if is a newline, if so, check if there is a macro after
            if self.nom_it::<_, _, ()>(line_ending).is_ok() {
                match self.parse_macro_line() {
                    Some(x) => return Ok(x),
                    None => {
                        //if there was a new line, but not macro, go back and clean
                        //the empty space on the start of the line and check again
                        continue;
                    }
                }
            } else {
                //not newline, it need to have a valid token, expansion or eof
                if self.data().is_empty() {
                    return Ok(DrainingFileBody::End);
                } else if let Ok((expansion, location)) = self.nom_it(expansion)
                {
                    return Ok(DrainingFileBody::Expansion(
                        expansion.to_owned(),
                        location,
                    ));
                } else {
                    return self
                        .nom_it(TokenType::parse)
                        .map(|(location, token_type)| {
                            Token::new(Span::File(token_type), location)
                        })
                        .map(DrainingFileBody::Token);
                }
            }
        }
    }
    fn parse(
        &mut self,
        state: &mut PreProcessorState,
    ) -> Result<DrainingFileProduct, Box<PreprocessorError>> {
        use DrainingFileBody::*;
        use MacroLine::*;
        loop {
            let status = self.if_stack.last().cloned();
            let next_block = self.parse_body()?;
            match (status, next_block) {
                (None, End) => return Ok(DrainingFileProduct::End),

                (Some((location, _)), End) => {
                    return Err(Box::new(PreprocessorError::NotFoundEndIf(
                        location,
                    )))
                }

                //found a token, just return it
                (_, Token(token)) => {
                    return Ok(DrainingFileProduct::Token(token))
                }

                // expand dong
                (_, Expansion(exp, src)) => {
                    return Ok(DrainingFileProduct::Expand(exp, src))
                }

                //include this sub-file
                (_, Macro(Include(file), src)) => {
                    return Ok(DrainingFileProduct::File(file, src))
                }

                (_, Macro(Define { name, value }, src)) => {
                    state.set_define(&name, value, src)?
                }
                (_, Macro(Undefine(name), src)) => {
                    //TODO ignore if try to delete non existing value?
                    if !state.del_value(&name) {
                        return Err(Box::new(
                            PreprocessorError::DeleteUndefined(src),
                        ));
                    }
                }

                //invalid block, such closing a block that was not open
                (None, Macro(ElIf(_), location))
                | (None, Macro(Else, location))
                | (None, Macro(EndIf, location))
                | (
                    Some((_, IfStatus::Else)),
                    Macro(ElIf(_) | Else, location),
                ) => {
                    return Err(Box::new(PreprocessorError::InvalidEndIf(
                        location,
                    )))
                }

                //end of the current block, remove the status and continue parsing
                (Some(_), Macro(EndIf, _)) => {
                    self.if_stack.pop();
                }

                //found a inner if, just push the block or skip it
                (_, Macro(If(cond), src)) => {
                    let cond = check_cond(state, cond, &src)?;
                    self.enter_if_block(cond, state, &src)?
                }

                //found the end of the currently executing if-block
                (
                    Some((_, IfStatus::If | IfStatus::IfElse)),
                    Macro(ElIf(_) | Else, src),
                ) => {
                    let _next_block = self.next_if_block(&src, |x| {
                        matches!(x, IfBorders::EndIf)
                    })?;
                    match self.nom_it(MacroLine::parse)? {
                        (MacroLine::EndIf, _) => (),
                        (_, _) => unreachable!(),
                    }
                    self.if_stack.pop();
                }
            }
        }
    }
}

fn check_cond(
    state: &PreProcessorState,
    cond: IfCheckOwned,
    src: &FileSpan,
) -> Result<bool, Box<PreprocessorError>> {
    match cond {
        IfCheckOwned::Defined(def) => Ok(state.exists(def.as_str())),
        IfCheckOwned::NotDefined(ndef) => Ok(!state.exists(ndef.as_str())),
        IfCheckOwned::Cmp { name, op, value } => {
            //TODO what if comparing a value that don't exists?
            let Some(defined) = state.get_value(name.as_str()) else {
                return Err(Box::new(PreprocessorError::ComparingUndefined(
                    src.clone(),
                )));
            };
            //TODO what if compring a define that have no value?
            let Some(defined_value) = &defined.value else {
                return Err(Box::new(PreprocessorError::ComparingEmpty(
                    src.clone(),
                )));
            };
            Ok(op.cmp(&value, defined_value))
        }
        IfCheckOwned::Op { left, op, right } => {
            let left = check_cond(state, *left, src)?;
            let right = check_cond(state, *right, src)?;
            Ok(op.check(left, right))
        }
    }
}

#[derive(Debug, Default)]
struct PreProcessorState(HashMap<Rc<str>, Define>);
impl PreProcessorState {
    fn exists(&self, ident: &str) -> bool {
        self.0.contains_key(ident)
    }
    fn get_value(&self, ident: &str) -> Option<&Define> {
        self.0.get(ident)
    }
    fn set_define(
        &mut self,
        name: &str,
        value: Option<DefineDataOwned>,
        src: FileSpan,
    ) -> Result<(), Box<PreprocessorError>> {
        use macros::DefineDataOwned::*;
        let (value, value_location) = match value {
            None => (None, src.clone()),
            Some(Alias(alias)) => {
                let (value, location) = self
                    .get_value(&alias)
                    .ok_or_else(|| {
                        Box::new(PreprocessorError::AliasUndefined(src.clone()))
                    })
                    .and_then(|define| {
                        let value = define.value.clone().ok_or_else(|| {
                            Box::new(PreprocessorError::AliasEmpty(src.clone()))
                        })?;
                        Ok((value, define.value_location.clone()))
                    })?;
                (Some(value), location)
            }
            Some(Value(value)) => (Some(value), src.clone()),
        };
        let define = Define {
            name: Rc::from(name),
            value,
            macro_location: src.clone(),
            value_location,
        };
        self.set_value(define);
        Ok(())
    }
    fn set_value(&mut self, define: Define) {
        let name = Rc::clone(&define.name);
        self.0.insert(name, define);
    }
    fn del_value(&mut self, ident: &str) -> bool {
        self.0.remove(ident).is_some()
    }
}

#[derive(Clone, Debug)]
pub enum DisplayToken {
    End,
    Concat,
    Ident(Span, String),
    Literal(String),
    Other(char),
}

#[derive(Debug)]
pub struct FilePreProcessor {
    pub root_path: Option<PathBuf>,
    file_stack: Vec<DrainingSource>,
    defines: PreProcessorState,
}
impl FilePreProcessor {
    pub fn new(file: &Path) -> Result<Self, Box<PreprocessorError>> {
        let root_path = file.parent().map(Path::to_path_buf);
        let file = DrainingFile::new(file)?;
        Ok(Self {
            root_path,
            defines: PreProcessorState::default(),
            file_stack: vec![DrainingSource::File(file)],
        })
    }
    pub fn is_finished(&self) -> bool {
        self.file_stack.is_empty()
    }
    /// process a display token
    pub fn parse_display(
        &mut self,
    ) -> Result<DisplayToken, Box<PreprocessorError>> {
        //TODO does the display should expand macros?
        //TODO does display can exist in between files? is this loop required?
        loop {
            let Some(file) = self.file_stack.last_mut() else {
                return Err(Box::new(PreprocessorError::UnclosedDisplay));
            };
            //try to parse a token from this source
            let token = match file {
                DrainingSource::File(file) => file.parse_display()?,
                DrainingSource::Macro(source) => source.parse_display()?,
            };
            if let Some(token) = token {
                return Ok(token);
            } else {
                //end of this file
                self.file_stack.pop();
            }
        }
    }
    /// process the accumulated buffer
    pub fn parse(&mut self) -> Result<Option<Token>, Box<PreprocessorError>> {
        // read files untill is able to process a token
        loop {
            let Some(file) = self.file_stack.last_mut() else {
                return Ok(None);
            };
            //try to parse a token from this source
            let token = match file {
                DrainingSource::File(file) => {
                    match file.parse(&mut self.defines)? {
                        DrainingFileProduct::End => None,
                        DrainingFileProduct::Token(token) => Some(token),
                        DrainingFileProduct::File(file, _src) => {
                            let fullpath = self
                                .root_path
                                .clone()
                                .unwrap_or_default()
                                .join(file);
                            self.file_stack.push(DrainingSource::File(
                                DrainingFile::new(&fullpath)?,
                            ));
                            continue;
                        }
                        DrainingFileProduct::Expand(exp, src) => {
                            let define = self
                                .defines
                                .get_value(&exp)
                                .ok_or_else(|| {
                                    Box::new(
                                        PreprocessorError::ExpandingUndefined(
                                            src.clone(),
                                        ),
                                    )
                                })?;
                            self.file_stack.push(DrainingSource::Macro(
                                DrainingMacro::new(define, src)?,
                            ));
                            continue;
                        }
                    }
                }
                DrainingSource::Macro(source) => source.parse()?,
            };
            //return this token, if unable, just pop it and goes to the next one
            match token {
                Some(token) => return Ok(Some(token)),
                None => {
                    self.file_stack.pop();
                }
            }
        }
    }
}
impl Iterator for FilePreProcessor {
    type Item = Result<Token, Box<PreprocessorError>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse().transpose()
    }
}
