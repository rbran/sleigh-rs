use crate::base::{empty_space0, ident};
use crate::IDENT_INSTRUCTION;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{consumed, cut, map, opt, value};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use super::disassembly::Disassembly;
use super::display::Display;
use super::execution::Execution;
use super::pattern::Pattern;

#[derive(Clone, Debug)]
pub struct Constructor<'a> {
    pub src: &'a str,
    pub table_name: Option<&'a str>,
    pub display: Display<'a>,
    pub pattern: Pattern<'a>,
    pub dissasembly: Option<Disassembly<'a>>,
    pub execution: Option<Execution<'a>>,
}

impl<'a> Constructor<'a> {
    pub fn table_name(&self) -> &'a str {
        self.table_name.unwrap_or(IDENT_INSTRUCTION)
    }
    pub fn is_root(&self) -> bool {
        self.table_name() == IDENT_INSTRUCTION
    }
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(tuple((
                terminated(opt(ident), pair(empty_space0, tag(":"))),
                cut(Display::parse),
                cut(preceded(empty_space0, Pattern::parse)),
                //disassembly is optional
                opt(preceded(
                    empty_space0,
                    delimited(
                        pair(tag("["), empty_space0),
                        Disassembly::parse,
                        pair(empty_space0, tag("]")),
                    ),
                )),
                //semantic could be empty or unimplemented (None)
                cut(preceded(
                    empty_space0,
                    alt((
                        value(None, tag("unimpl")),
                        delimited(
                            pair(tag("{"), empty_space0),
                            map(Execution::parse, |x| Some(x)),
                            pair(empty_space0, tag("}")),
                        ),
                    )),
                )),
            ))),
            |(src, (table_name, display, pattern, dissasembly, semantic))| {
                Self {
                    src,
                    table_name,
                    display,
                    pattern,
                    execution: semantic,
                    dissasembly,
                }
            },
        )(input)
    }
}

#[cfg(test)]
mod test {
    //    use nom::combinator::eof;
    //    use nom::sequence::terminated;
    //
    //    use crate::processor::table::dissasembly::{
    //        Assignment as AssignmentDis, DissasemblyAction, ExprElement as ExprDis,
    //        Op as OpDis,
    //    };
    //    use crate::processor::table::pattern::{
    //        BoolOp, CmpOp, ConstraintValue, Op as OpPat, Pattern,
    //        PatternElement as ExprPat,
    //    };
    //    use crate::processor::table::semantic::Value;
    //    use crate::processor::table::{
    //        table_constructor_def, DisplayElement, SemanticParser,
    //        TableConstructorDef,
    //    };
    //
    //    #[test]
    //    fn test_table1() {
    //        let test = "ItCond:            is TMode=1 & itmode=0 & cond_mask=0 {}";
    //        let (_, table) = terminated(table_constructor_def, eof)(test).unwrap();
    //        assert_eq!(
    //            table,
    //            TableConstructorDef {
    //                table_identifier: Some("ItCond".to_string()),
    //                display: vec![],
    //                pattern: Pattern {
    //                    stack: vec![
    //                        ExprPat::Field {
    //                            constraint: "TMode".into(),
    //                            op: CmpOp::Eq,
    //                            value: ConstraintValue::Value(1),
    //                        },
    //                        ExprPat::Field {
    //                            constraint: "itmode".into(),
    //                            op: CmpOp::Eq,
    //                            value: ConstraintValue::Value(0),
    //                        },
    //                        ExprPat::Op(OpPat::Bool(BoolOp::And)),
    //                        ExprPat::Field {
    //                            constraint: "cond_mask".into(),
    //                            op: CmpOp::Eq,
    //                            value: ConstraintValue::Value(0),
    //                        },
    //                        ExprPat::Op(OpPat::Bool(BoolOp::And)),
    //                    ]
    //                },
    //                dissasembly: None,
    //                semantic: SemanticParser::default(),
    //            }
    //        );
    //    }
    //    #[test]
    //    fn test_table2() {
    //        use DisplayElement::*;
    //        let test = "ItCond: Reg1 (Reg2),  [Reg3]  is \
    //                    Reg1  &(Op1=2 | Op2=3 | Op3  =4) & Reg2 & Reg3 \
    //                    [cond_shft=cond_shft << 1; globalset(reloc,TMode);] {}";
    //        let (_, table) = terminated(table_constructor_def, eof)(test).unwrap();
    //        assert_eq!(
    //            table,
    //            TableConstructorDef {
    //                table_identifier: Some("ItCond".to_string()),
    //                display: vec![
    //                    Pattern("Reg1".into()),
    //                    Literal(" (".into()),
    //                    Pattern("Reg2".into()),
    //                    Literal("), [".into()),
    //                    Pattern("Reg3".into()),
    //                    Literal("]".into()),
    //                ],
    //                pattern: Pattern {
    //                    stack: vec![
    //                        ExprPat::Ident("Reg1".into()),
    //                        ExprPat::Field {
    //                            constraint: "Op1".into(),
    //                            op: CmpOp::Eq,
    //                            value: ConstraintValue::Value(2),
    //                        },
    //                        ExprPat::Field {
    //                            constraint: "Op2".into(),
    //                            op: CmpOp::Eq,
    //                            value: ConstraintValue::Value(3),
    //                        },
    //                        ExprPat::Op(OpPat::Bool(BoolOp::Or)),
    //                        ExprPat::Field {
    //                            constraint: "Op3".into(),
    //                            op: CmpOp::Eq,
    //                            value: ConstraintValue::Value(4),
    //                        },
    //                        ExprPat::Op(OpPat::Bool(BoolOp::Or)),
    //                        ExprPat::Op(OpPat::Bool(BoolOp::And)),
    //                        ExprPat::Ident("Reg2".into()),
    //                        ExprPat::Op(OpPat::Bool(BoolOp::And)),
    //                        ExprPat::Ident("Reg3".into()),
    //                        ExprPat::Op(OpPat::Bool(BoolOp::And)),
    //                    ]
    //                },
    //                dissasembly: Some(DissasemblyAction {
    //                    assignments: vec![
    //                        AssignmentDis::Local {
    //                            left: "cond_shft".into(),
    //                            right: vec![
    //                                ExprDis::Value(Value::Var("cond_shft".into())),
    //                                ExprDis::Value(Value::Int(1)),
    //                                ExprDis::Op(OpDis::Lsl)
    //                            ],
    //                        },
    //                        AssignmentDis::Global("reloc".into(), "TMode".into()),
    //                    ]
    //                }),
    //                semantic: SemanticParser::default(),
    //            },
    //        );
    //    }
}
