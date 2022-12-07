mod alignment;
mod bitrange;
mod context;
mod endian;
mod space;
mod token;
mod user_function;
mod varnode;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{cut, map};
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;

use crate::base::{empty_space0, empty_space1};

pub use self::alignment::Alignment;
pub use self::bitrange::{BitRangeDef, VarnodeField};
pub use self::context::{Context, ContextField, ContextFieldAttribute};
pub use self::space::{Attribute, Space};
pub use self::token::{Token, TokenField, TokenFieldAttribute};
pub use self::user_function::UserFunction;
pub use self::varnode::Varnode;

pub use crate::semantic::Endian;

#[derive(Clone, Debug)]
pub enum Define<'a> {
    Endian(Endian),
    Alignment(Alignment),
    Varnode(Varnode<'a>),
    Bitrange(BitRangeDef<'a>),
    Token(Token<'a>),
    UserFunction(UserFunction<'a>),
    Context(Context<'a>),
    Space(Space<'a>),
}

impl<'a> Define<'a> {
    pub fn parse(input: &str) -> IResult<&str, Define> {
        preceded(
            pair(tag("define"), empty_space1),
            cut(terminated(
                alt((
                    map(Endian::parse, |x| Define::Endian(x)),
                    map(Alignment::parse, |x| Define::Alignment(x)),
                    map(UserFunction::parse, |x| Define::UserFunction(x)),
                    map(Space::parse, |x| Define::Space(x)),
                    map(Token::parse, |x| Define::Token(x)),
                    map(BitRangeDef::parse, |x| Define::Bitrange(x)),
                    map(Context::parse, |x| Define::Context(x)),
                    //NOTE Varnode need to be the last
                    map(Varnode::parse, |x| Define::Varnode(x)),
                )),
                pair(empty_space0, tag(";")),
            )),
        )(input)
    }
}

#[cfg(test)]
mod test {
    //    use crate::processor::define::{
    //        address_space_def, attach_names_def, attach_values_def,
    //        attach_variables_def, bitranges_def, context_def_old, token_def,
    //        varnode_def, AddressSpaceDef, AddressSpaceType, BitrangeDef,
    //        ContextDef, ContextFieldDef, Endian, TokenDef, TokenFieldDef,
    //        VarnodeGrupDef,
    //    };
    //
    //    #[test]
    //    fn test_space_address_def() {
    //        assert_eq!(
    //            address_space_def(
    //                "define space ram_name type=ram_space size=4 default;"
    //            )
    //            .unwrap()
    //            .1,
    //            AddressSpaceDef {
    //                name: "ram_name".to_string(),
    //                address_space_type: AddressSpaceType::Ram,
    //                size: 4,
    //                wordsize: 8,
    //                default: true,
    //            }
    //        );
    //        assert_eq!(
    //            address_space_def(
    //                "define space register_name type=register_space size=4;"
    //            )
    //            .unwrap()
    //            .1,
    //            AddressSpaceDef {
    //                name: "register_name".to_string(),
    //                address_space_type: AddressSpaceType::Register,
    //                size: 4,
    //                wordsize: 8,
    //                default: false,
    //            }
    //        );
    //    }
    //
    //    #[test]
    //    fn test_varnode_def() {
    //        assert_eq!(
    //            varnode_def(
    //                "define register offset=0 size=4\n\t\
    //                [EAX ECX EDX EBX ESP EBP ESI EDI ];"
    //            )
    //            .unwrap()
    //            .1,
    //            VarnodeGrupDef {
    //                address_space_name: "register".to_string(),
    //                offset: 0,
    //                size: 4,
    //                varnodes: vec![
    //                    Some("EAX".into()),
    //                    Some("ECX".into()),
    //                    Some("EDX".into()),
    //                    Some("EBX".into()),
    //                    Some("ESP".into()),
    //                    Some("EBP".into()),
    //                    Some("ESI".into()),
    //                    Some("EDI".into())
    //                ],
    //            }
    //        );
    //        assert_eq!(
    //            varnode_def(
    //                "define register2 offset=0 size=2\n\
    //                [AX _ CX _ DX _ BX _ SP _ BP _ SI _ DI];"
    //            )
    //            .unwrap()
    //            .1,
    //            VarnodeGrupDef {
    //                address_space_name: "register2".to_string(),
    //                offset: 0,
    //                size: 2,
    //                varnodes: vec![
    //                    Some("AX".into()),
    //                    None,
    //                    Some("CX".into()),
    //                    None,
    //                    Some("DX".into()),
    //                    None,
    //                    Some("BX".into()),
    //                    None,
    //                    Some("SP".into()),
    //                    None,
    //                    Some("BP".into()),
    //                    None,
    //                    Some("SI".into()),
    //                    None,
    //                    Some("DI".into())
    //                ],
    //            }
    //        );
    //    }
    //    #[test]
    //    fn test_bitrange_def() {
    //        assert_eq!(
    //            bitranges_def(
    //                "define bitrange zf=statusreg[10,1]\n\
    //                \tcf=statusreg[11,1]\n\
    //                sf=statusreg[12,2] ;"
    //            )
    //            .unwrap()
    //            .1,
    //            vec![
    //                BitrangeDef {
    //                    name: "zf".into(),
    //                    varnode_name: "statusreg".into(),
    //                    index: 10,
    //                    size: 1,
    //                },
    //                BitrangeDef {
    //                    name: "cf".into(),
    //                    varnode_name: "statusreg".into(),
    //                    index: 11,
    //                    size: 1,
    //                },
    //                BitrangeDef {
    //                    name: "sf".into(),
    //                    varnode_name: "statusreg".into(),
    //                    index: 12,
    //                    size: 2,
    //                },
    //            ]
    //        );
    //    }
    //    #[test]
    //    fn test_token_def() {
    //        assert_eq!(
    //            token_def(
    //                "define token instr ( 32 ) endian=little\n\
    //                    op0=(0,1)\n\
    //                    op1 =(2,3) signed\n\
    //                    op2= (4,5) hex\n\
    //                    op3=(6, 7) dec\n\
    //                    op4=(8,9) signed dec\n\
    //                    op5=(10,12) dec signed;"
    //            )
    //            .unwrap()
    //            .1,
    //            TokenDef {
    //                name: "instr".into(),
    //                size: 32,
    //                endian: Some(Endian::Little),
    //                field_defs: vec![
    //                    TokenFieldDef {
    //                        name: "op0".into(),
    //                        start: 0,
    //                        end: 1,
    //                        signed: false,
    //                        hex: false,
    //                        dec: false,
    //                    },
    //                    TokenFieldDef {
    //                        name: "op1".into(),
    //                        start: 2,
    //                        end: 3,
    //                        signed: true,
    //                        hex: false,
    //                        dec: false,
    //                    },
    //                    TokenFieldDef {
    //                        name: "op2".into(),
    //                        start: 4,
    //                        end: 5,
    //                        signed: false,
    //                        hex: true,
    //                        dec: false,
    //                    },
    //                    TokenFieldDef {
    //                        name: "op3".into(),
    //                        start: 6,
    //                        end: 7,
    //                        signed: false,
    //                        hex: false,
    //                        dec: true,
    //                    },
    //                    TokenFieldDef {
    //                        name: "op4".into(),
    //                        start: 8,
    //                        end: 9,
    //                        signed: true,
    //                        hex: false,
    //                        dec: true,
    //                    },
    //                    TokenFieldDef {
    //                        name: "op5".into(),
    //                        start: 10,
    //                        end: 12,
    //                        signed: true,
    //                        hex: false,
    //                        dec: true,
    //                    },
    //                ],
    //            }
    //        );
    //    }
    //    #[test]
    //    fn test_context_def() {
    //        assert_eq!(
    //            context_def_old(
    //                "define context contextreg\n\
    //                    op0=(0,1)\n\
    //                    op1 =(2,3) signed noflow\n\
    //                    op2= (4,5) hex\n\
    //                    op3=(6, 7) dec\n\
    //                    op4=(8,9) signed dec noflow\n\
    //                    op5=(10,12) dec signed noflow;"
    //            )
    //            .unwrap()
    //            .1,
    //            ContextDef {
    //                varnode_name: "contextreg".into(),
    //                field_defs: vec![
    //                    ContextFieldDef {
    //                        name: "op0".into(),
    //                        start: 0,
    //                        end: 1,
    //                        signed: false,
    //                        hex: false,
    //                        dec: false,
    //                        noflow: false,
    //                    },
    //                    ContextFieldDef {
    //                        name: "op1".into(),
    //                        start: 2,
    //                        end: 3,
    //                        signed: true,
    //                        hex: false,
    //                        dec: false,
    //                        noflow: true,
    //                    },
    //                    ContextFieldDef {
    //                        name: "op2".into(),
    //                        start: 4,
    //                        end: 5,
    //                        signed: false,
    //                        hex: true,
    //                        dec: false,
    //                        noflow: false,
    //                    },
    //                    ContextFieldDef {
    //                        name: "op3".into(),
    //                        start: 6,
    //                        end: 7,
    //                        signed: false,
    //                        hex: false,
    //                        dec: true,
    //                        noflow: false,
    //                    },
    //                    ContextFieldDef {
    //                        name: "op4".into(),
    //                        start: 8,
    //                        end: 9,
    //                        signed: true,
    //                        hex: false,
    //                        dec: true,
    //                        noflow: true,
    //                    },
    //                    ContextFieldDef {
    //                        name: "op5".into(),
    //                        start: 10,
    //                        end: 12,
    //                        signed: true,
    //                        hex: false,
    //                        dec: true,
    //                        noflow: true,
    //                    },
    //                ],
    //            }
    //        );
    //    }
    //    #[test]
    //    fn test_attach_variables_def() {
    //        assert_eq!(
    //            attach_variables_def("attach variables [A B Cc] [_ a _ b C] ;")
    //                .unwrap()
    //                .1,
    //            (
    //                vec!["A".into(), "B".into(), "Cc".into()],
    //                vec![
    //                    None,
    //                    Some("a".into()),
    //                    None,
    //                    Some("b".into()),
    //                    Some("C".into())
    //                ]
    //            )
    //        )
    //    }
    //    #[test]
    //    fn test_attach_names_def() {
    //        assert_eq!(
    //            attach_names_def("attach names [A B Cc] [AA BB CC] ;")
    //                .unwrap()
    //                .1,
    //            (
    //                vec!["A".into(), "B".into(), "Cc".into()],
    //                vec![Some("AA".into()), Some("BB".into()), Some("CC".into())]
    //            )
    //        )
    //    }
    //    #[test]
    //    fn test_attach_values_def() {
    //        assert_eq!(
    //            attach_values_def("attach values B [-1 2 100 _] ;")
    //                .unwrap()
    //                .1,
    //            (vec!["B".into()], vec![Some(-1), Some(2), Some(100), None])
    //        )
    //    }
}
