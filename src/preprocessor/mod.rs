pub mod ifs;
pub mod macros;

use thiserror::Error;

use indexmap::IndexMap;
use std::cmp::Ordering;
use std::fs::File;
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::InputSource;

use self::ifs::IfCheck;
use self::macros::Block;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("IO Error {0}")]
    File(#[from] std::io::Error),
    #[error("Preprocessor Parsing error at\n{0}\n")]
    Parse(#[from] nom::Err<nom::error::Error<InputSource>>),
    #[error("Preprocessor Execute at {message} {src}")]
    Execute {
        message: &'static str,
        src: InputSource,
    },
}

#[derive(Debug)]
struct ExecuteError<'a> {
    message: &'static str,
    src: &'a str,
}

struct ParsedFile {
    filename: Rc<Path>,
    data: String,
    //NOTE: 'static is just for the LoLs: it points to &self.data
    //NOTE blocks will point to data, so be carefull to only drop data if blocks
    //are also dropped
    blocks: Vec<Block<'static>>,
}

pub struct Parser {
    files: IndexMap<Rc<Path>, Rc<ParsedFile>>,
    root_path: PathBuf,
    //TODO use Rc<str> here and in Block to avoid duplication?
    defines: IndexMap<String, String>,
}

impl Parser {
    pub fn new(root_path: PathBuf) -> Self {
        Self {
            files: IndexMap::default(),
            root_path,
            defines: IndexMap::default(),
        }
    }
    pub fn preprocess(
        root_path: PathBuf,
        filename: &Path,
    ) -> Result<PreProcOutput, ParseError> {
        let mut parse = Self::new(root_path);
        //parse the main file
        let file = parse.parse(filename)?;

        //execute this root block, and put the output on this stack
        let mut stack = PreProcOutput::default();
        parse.exec(&file.data, &file.filename, &file.blocks, &mut stack)?;
        Ok(stack)
    }
    fn parse(&mut self, filename: &Path) -> Result<Rc<ParsedFile>, ParseError> {
        if let Some(blocks) = self.files.get(filename) {
            return Ok(Rc::clone(blocks));
        }
        let filename = self.root_path.join(filename);
        let mut file = File::open(&filename).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data)?;
        //NOTE to avoid some parse problems, all files must end with \n
        //match data.bytes().last() {
        //    Some(b'\n') | Some(b'\r') => (),
        //    None => (),
        //    Some(_) => data.push('\n'),
        //}
        let filename = Rc::from(filename);
        let blocks = macros::parse_blocks(&data)
            //convert to nom::Err<nom::error::Error<InputSource>>
            .map_err(|nom_err| {
                nom_err.map_input(|input| {
                    InputSource::new_from_offset(
                        &data,
                        input,
                        Rc::clone(&filename),
                    )
                })
            })?
            .1;
        //NOTE: As long this goes into ParsedFile and not modified, the 'static
        //will be safe
        let blocks = unsafe { std::mem::transmute(blocks) };
        let file = Rc::new(ParsedFile {
            filename: Rc::clone(&filename),
            data,
            blocks,
        });
        self.files.insert(filename, Rc::clone(&file));
        Ok(file)
    }
    fn get_value<'a>(&'a self, ident: &str) -> Option<&'a str> {
        self.defines.get(ident).map(|x| x.as_str())
    }
    fn set_value(&mut self, ident: String, value: String) {
        self.defines.insert(ident, value);
    }
    fn del_value(&mut self, ident: &str) {
        self.defines.remove(ident);
    }
    fn exec(
        &mut self,
        data: &str,
        filename: &Rc<Path>,
        blocks: &[Block],
        stack: &mut PreProcOutput,
    ) -> Result<(), ParseError> {
        use Block::*;
        let get_src =
            |x| InputSource::new_from_offset(data, x, Rc::clone(filename));
        for block in blocks.iter() {
            match block {
                Data(data) => {
                    stack.add(data, get_src(data));
                }
                Expand(name) => {
                    //TODO: what if define is alias? Get the original value?
                    let value =
                        self.get_value(name).ok_or(ParseError::Execute {
                            message: "Expansion not found",
                            src: get_src(name),
                        })?;
                    //TODO remove this hack
                    let value = format!(" {} ", value);
                    stack.add(&value, get_src(name));
                }
                Define { name, value, src } => {
                    use macros::DefineData::*;
                    let (name, value): (&str, &str) = match value {
                        None => (name, ""),
                        Some(Value(value)) => (name, value),
                        Some(Alias(value_name)) => {
                            let value = self.get_value(value_name).ok_or(
                                ParseError::Execute {
                                    message: "Defined Value not found",
                                    src: get_src(src),
                                },
                            )?;
                            (name, &value)
                        }
                    };
                    let name = name.to_owned();
                    let value = value.to_owned();
                    self.set_value(name, value);
                }
                Undefine(name) => {
                    //TODO what to do if value don't exists? Just ignore it?
                    self.del_value(name);
                }
                Include(file) => {
                    let mut filename = self.root_path.to_path_buf();
                    filename.push(file);
                    //parse this file, and exec it before continue
                    let file = self.parse(&filename)?;
                    self.exec(&file.data, &file.filename, &file.blocks, stack)?;
                }
                Cond(cond) => {
                    let block = cond.blocks.iter().find_map(
                        |(cond, blocks)| match self.check_cond(&cond) {
                            Err(e) => Some(Err(e)),
                            Ok(true) => Some(Ok(blocks)),
                            Ok(false) => None,
                        },
                    );
                    match block {
                        None => {
                            self.exec(data, filename, &cond.else_block, stack)?
                        }
                        Some(block) => {
                            let block =
                                block.map_err(|e| ParseError::Execute {
                                    message: e.message,
                                    src: get_src(e.src),
                                })?;
                            self.exec(data, filename, &block, stack)?
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn check_cond<'a>(
        &self,
        cond: &IfCheck<'a>,
    ) -> Result<bool, ExecuteError<'a>> {
        use IfCheck::*;
        let value = match cond {
            Defined(name) => self.get_value(name).is_some(),
            NotDefined(name) => self.get_value(name).is_none(),
            Cmp {
                name,
                op,
                value,
                src,
            } => match self.get_value(name) {
                None => {
                    return Err(ExecuteError {
                        message: "Value not found",
                        src,
                    })
                }
                Some(value1) => op.cmp(value1, value),
            },
            Op { left, op, right } => {
                op.check(self.check_cond(left)?, self.check_cond(right)?)
            }
        };
        Ok(value)
    }
}

#[derive(Debug, Clone, Default)]
pub struct PreProcOutput {
    src: Vec<(Range<usize>, InputSource)>,
    output: String,
}

impl PreProcOutput {
    pub fn as_str(&self) -> &str {
        &self.output
    }
    pub fn add(&mut self, data: &str, src: InputSource) {
        let start = self.output.len();
        self.output.push_str(data);
        let end = self.output.len();
        let range = start..end;
        self.src.push((range, src));
    }
    pub fn source_data_start<'a>(
        &'a self,
        find: &'a str,
    ) -> Option<InputSource> {
        let output_offset = self.output.as_ptr() as usize;
        let find_offset = find.as_ptr() as usize;
        assert!(find_offset >= output_offset);
        let output_position = find_offset - output_offset;
        self.src
            .binary_search_by(|(range, _)| match (range.start, range.end) {
                (start, _) if output_position < start => Ordering::Greater,
                (_, end) if output_position >= end => Ordering::Less,
                (_, _) => Ordering::Equal,
            })
            .ok()
            .map(|pos| &self.src[pos])
            .map(|(range, InputSource { line, column, file })| {
                //TODO crimes against humanity here
                let current_chunk = &self.output[range.start..output_position];
                let file = Rc::clone(file);
                let mut line = *line;
                let column = current_chunk
                    .split('\n')
                    .inspect(|_| line += 1)
                    .last()
                    .map(|x| 1 + x.chars().count())
                    .unwrap_or(*column);
                InputSource { line, column, file }
            })
    }
}

pub fn preprocess(file: &Path) -> Result<PreProcOutput, ParseError> {
    let error = |x| std::io::Error::new(std::io::ErrorKind::InvalidData, x);
    let parent = file
        .parent()
        .ok_or_else(|| error("Unable to solve file path"))?;
    let filename = file
        .file_name()
        .ok_or_else(|| error("Unable to solve file name"))?;

    Parser::preprocess(parent.to_path_buf(), Path::new(filename))
}
