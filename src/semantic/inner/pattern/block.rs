use std::convert::TryFrom;

use indexmap::IndexMap;

use crate::semantic::inner::disassembly::Assertation;
use crate::semantic::inner::pattern::ConstraintValue;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::{GlobalScope, Sleigh, SolverStatus};
use crate::semantic::inner::token::TokenField;
use crate::semantic::pattern::{Ellipsis, PatternError, PatternLen};
use crate::semantic::token::Token;
use crate::semantic::{GlobalAnonReference, GlobalReference};
use crate::syntax::block;
use crate::syntax::block::pattern::Op;
use crate::Span;

use super::constraint::{BitConstraint, BlockConstraint};
use super::{
    is_len_finished, ConstructorPatternLen, ProducedTable,
    ProducedTokenField, Verification, Pattern, FindValues,
};
pub type FinalBlock = crate::semantic::pattern::Block;
#[derive(Clone, Debug)]
pub struct Block {
    pub op: Op,
    pub location: Span,
    pub len: Option<ConstructorPatternLen>,
    //root_len: usize,

    //block produces this token this number of times
    pub tokens: IndexMap<*const Token, (usize, GlobalAnonReference<Token>)>,
    //map to make sure token_fields are produced only once
    //fields extracted, implicitly or explicity, localy or in sub_pattern
    pub token_fields: IndexMap<*const TokenField, ProducedTokenField>,
    //produced tables
    pub tables: IndexMap<*const Table, ProducedTable>,

    //verification in the order they are defined
    pub verifications: Vec<Verification>,
    pub pre: Vec<Assertation>,
    pub pos: Vec<Assertation>,
}

impl Block {
    fn new_or<'a>(
        sleigh: &Sleigh,
        location: Span,
        elements: impl Iterator<Item = block::pattern::Element>,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        //convert the verifications and generate the tokens/token_fields that
        //all branches produces.
        let mut tokens: Option<IndexMap<_, (usize, _)>> = None;
        let mut token_fields: Option<IndexMap<_, _>> = None;
        let branches = elements
            //convert into Verifications
            .map(|element| {
                let block::pattern::Element { field, ellipsis } = element;
                if ellipsis.is_some() {
                    //TODO error here
                    todo!("Ellipsis on OR pattern?");
                }
                match field {
                    block::pattern::Field::Field {
                        field,
                        src,
                        constraint: None,
                    } => {
                        let field = sleigh
                            .get_global(&field)
                            .ok_or(PatternError::MissingRef(src.clone()))?;
                        match field {
                            GlobalScope::Table(table) => {
                                //this branch only produces a table, so no
                                //token/token_fields, so clean/empty it
                                match &mut tokens {
                                    Some(tokens) => tokens.clear(),
                                    None => tokens = Some(IndexMap::new()),
                                }
                                match &mut token_fields {
                                    Some(token_fields) => token_fields.clear(),
                                    None => {
                                        token_fields = Some(IndexMap::new())
                                    }
                                }
                                Ok(Verification::new_table(
                                    this_table, table, src, None,
                                ))
                            }
                            _ => Err(PatternError::UnrestrictedOr(src)),
                        }
                    }
                    block::pattern::Field::Field {
                        field,
                        src,
                        constraint: Some(constraint),
                    } => {
                        let verification = Verification::from_constraint(
                            sleigh, &field, &src, constraint, this_table,
                        )?;
                        match &verification {
                            Verification::ContextCheck { .. }
                            | Verification::TableBuild { .. } => {
                                //this branch only produces_table/check_context,
                                //so no token/token_fields, so clean/empty it
                                match &mut tokens {
                                    Some(tokens) => tokens.clear(),
                                    None => tokens = Some(IndexMap::new()),
                                }
                                match &mut token_fields {
                                    Some(token_fields) => token_fields.clear(),
                                    None => {
                                        token_fields = Some(IndexMap::new())
                                    }
                                }
                            }
                            Verification::TokenFieldCheck { field, .. } => {
                                let field_ele = field.element();
                                let token = &field_ele.token;
                                //this branch checks a token_field, remove all
                                //tokens except for this one
                                match &mut tokens {
                                    Some(tokens) => {
                                        tokens.retain(|this_token, _| {
                                            *this_token == token.element_ptr()
                                        })
                                    }
                                    None => {
                                        tokens = Some(IndexMap::from([(
                                            token.element_ptr(),
                                            (1, token.reference()),
                                        )]))
                                    }
                                }
                                //to token_fields are produced, clean/empty it
                                match &mut token_fields {
                                    Some(token_fields) => token_fields.clear(),
                                    None => {
                                        token_fields = Some(IndexMap::new())
                                    }
                                }
                            }
                            Verification::SubPattern { .. } => {
                                unreachable!()
                            }
                        }
                        Ok(verification)
                    }
                    block::pattern::Field::SubPattern(sub) => {
                        let pattern = Pattern::new(sleigh, sub, this_table)?;
                        //remote all the tokens that this sub_pattern don't
                        //produces or produces more then once
                        match &mut tokens {
                            Some(tokens) => {
                                tokens.retain(|this_token, (num, _token)| {
                                    let found = pattern.tokens.get(this_token);
                                    if let Some((found_num, _token)) = found {
                                        *num = (*num).max(*found_num);
                                    }
                                    found.is_some()
                                })
                            }
                            None => tokens = Some(pattern.tokens.clone()),
                        }
                        //remove all token this sub_pattern, don't produces
                        match &mut token_fields {
                            Some(fields) => fields.retain(|this_field, _| {
                                pattern.token_fields.contains_key(this_field)
                            }),
                            None => {
                                token_fields = Some(
                                    pattern
                                        .token_fields
                                        .iter()
                                        .map(|(k, v)| {
                                            let mut v = v.clone();
                                            v.local = false;
                                            (*k, v)
                                        })
                                        .collect(),
                                )
                            }
                        }
                        Ok(Verification::SubPattern {
                            location: location.clone(),
                            pattern,
                        })
                    }
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        //all tables produced should be included on the final tables list,
        //but mark always=true only tables produced in all branches
        let mut tables_iter = branches.iter();
        let mut tables: IndexMap<*const Table, ProducedTable> =
            match tables_iter.next().expect("LOGIC_ERROR: empty OR pattern") {
                //no tables in branch/no_branch
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. } => IndexMap::new(),
                //branch produces only one table, add this table
                Verification::TableBuild {
                    produced_table,
                    verification: _,
                } => IndexMap::from([(
                    produced_table.table.element_ptr(),
                    produced_table.clone(),
                )]),
                Verification::SubPattern {
                    location: _,
                    pattern,
                } => pattern.tables.clone(),
            };
        tables_iter.for_each(|verification| {
            match verification {
                //no table in this branch, mark all existing tables as
                //not always producing.
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. } => {
                    tables.values_mut().for_each(|table| table.always = false)
                }

                //branch produces only one table, add this table
                Verification::TableBuild {
                    produced_table,
                    verification: _,
                } => {
                    let ptr = produced_table.table.element_ptr();
                    //if this table don't exists, add it and mark it
                    //not_always produce
                    tables.entry(ptr).or_insert_with(|| {
                        let mut produced_table = produced_table.clone();
                        produced_table.always = false;
                        produced_table
                    });
                    //mark all other tables as not always produce
                    tables
                        .iter_mut()
                        .filter(|(k, _v)| **k != ptr)
                        .for_each(|(_k, v)| v.always = false);
                }
                //this branch can produce 0..n tables
                Verification::SubPattern {
                    location: _,
                    pattern,
                } => {
                    pattern.tables.iter().for_each(|(ptr, produced_table)| {
                        //if this table don't exists, add it and mark it
                        //not_always produce
                        tables.entry(*ptr).or_insert_with(|| {
                            let mut produced_table = produced_table.clone();
                            produced_table.always = false;
                            produced_table
                        });
                    });
                    //mark all other tables as not always produce
                    tables
                        .iter_mut()
                        .filter(|(k, _v)| !pattern.tables.contains_key(*k))
                        .for_each(|(_k, v)| v.always = false);
                }
            }
        });

        //tokens that are present in all branches of the or can produce
        //fields,
        let mut tokens_iter = branches.iter();
        let mut tokens: IndexMap<
            *const Token,
            (usize, GlobalAnonReference<Token>),
        > = match tokens_iter.next().expect("LOGIC_ERROR: empty OR pattern") {
            //no fields exported
            Verification::ContextCheck { .. }
            | Verification::TableBuild { .. } => IndexMap::new(),
            //TODO: sub_pattern export tokens?
            Verification::SubPattern { .. } => IndexMap::new(),
            //one fields exported
            Verification::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => {
                let token_field = field.element();
                let token = &token_field.token;
                IndexMap::from([(token.element_ptr(), (1, token.reference()))])
            }
        };
        for verification in tokens_iter {
            match verification {
                //no fields, just clean it
                Verification::ContextCheck { .. }
                | Verification::TableBuild { .. } => {
                    tokens.clear();
                }
                //TODO: sub_pattern export tokens?
                Verification::SubPattern { .. } => tokens.clear(),
                //one field, remove all the others
                Verification::TokenFieldCheck {
                    field,
                    op: _,
                    value: _,
                } => {
                    let token_field = field.element();
                    tokens.retain(|ptr, _token| {
                        *ptr == token_field.token.element_ptr()
                    })
                }
            }
            if tokens.is_empty() {
                //don't need to search any further, because is already empty
                break;
            }
        }
        //fields explicitly produced by all branches, are produced by the or
        //pattern
        let mut fields_iter = branches.iter();
        let mut fields: IndexMap<*const TokenField, ProducedTokenField> =
            match fields_iter.next().expect("LOGIC_ERROR: empty OR pattern") {
                //no fields exported
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. }
                | Verification::TableBuild { .. } => IndexMap::new(),
                //TODO: sub_pattern export tokens?
                Verification::SubPattern { .. } => IndexMap::new(),
                //one fields exported
            };
        for verification in fields_iter {
            match verification {
                //no fields, just clean it
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. }
                | Verification::TableBuild { .. } => {
                    fields.clear();
                }
                Verification::SubPattern {
                    location: _,
                    pattern,
                } => fields.retain(|ptr, _token| {
                    pattern.token_fields.keys().any(|sub_ptr| sub_ptr == ptr)
                }),
            }
            if fields.is_empty() {
                //don't need to search any further, because is already empty
                break;
            }
        }
        fields.values_mut().for_each(|prod| prod.local = false);

        Ok(Self {
            op: Op::Or,
            location,
            len: None,
            tokens,
            token_fields: token_fields.unwrap_or_default(),
            tables,
            verifications: branches,
            pre: vec![],
            pos: vec![],
        })
    }
    fn new_and<'a>(
        sleigh: &Sleigh,
        location: Span,
        elements: impl Iterator<Item = block::pattern::Element>,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        //convert into Verifications, also capturing the explicit fields
        let mut token_fields = IndexMap::new();
        use indexmap::map::Entry::*;
        //closure for identation sake
        let mut add_explicit_token_field =
            |token_field: ProducedTokenField| match token_fields
                .entry(token_field.field.element_ptr())
            {
                Occupied(entry) => {
                    let field_old: &ProducedTokenField = entry.get();
                    Err(PatternError::MultipleProduction(
                        field_old.field.location().clone(),
                        token_field.field.location().clone(),
                    ))
                }
                Vacant(entry) => {
                    entry.insert(token_field);
                    Ok(())
                }
            };
        let mut verifications = vec![];
        for element in elements {
            let block::pattern::Element { field, ellipsis } = element;
            if matches!(ellipsis, Some(Ellipsis::Left)) {
                todo!("Ellispsis on the Left")
            }
            match field {
                block::pattern::Field::Field {
                    field,
                    constraint: Some(constraint),
                    src,
                } => {
                    let block::pattern::Constraint { op: cmp_op, value } =
                        constraint;
                    let value = ConstraintValue::new(sleigh, value)?;
                    let field = sleigh
                        .get_global(&field)
                        .ok_or(PatternError::MissingRef(src.clone()))?;
                    match field {
                        GlobalScope::TokenField(x) => {
                            verifications.push(Verification::new_token_field(
                                x, src, cmp_op, value,
                            ))
                        }
                        //TODO create InstStart? Does start_start exists?
                        GlobalScope::Context(x) => verifications.push(
                            Verification::new_context(x, src, cmp_op, value),
                        ),
                        GlobalScope::Table(x) => {
                            verifications.push(Verification::new_table(
                                this_table,
                                x,
                                src,
                                Some((cmp_op, value)),
                            ))
                        }
                        _ => return Err(PatternError::InvalidRef(src)),
                    };
                }

                block::pattern::Field::Field {
                    field,
                    constraint: None,
                    src,
                } => {
                    let field = sleigh
                        .get_global(&field)
                        .ok_or(PatternError::MissingRef(src.clone()))?;
                    match field {
                        //could be explicitly defined, usually for display,
                        //but is not required
                        GlobalScope::Epsilon(_)
                        | GlobalScope::Varnode(_)
                        | GlobalScope::Context(_)
                        | GlobalScope::Bitrange(_) => (),
                        //token_field exported
                        GlobalScope::TokenField(token_field) => {
                            add_explicit_token_field(ProducedTokenField {
                                explicit: true,
                                local: true,
                                field: token_field.reference_from(src),
                            })?
                        }
                        GlobalScope::Table(table) => {
                            verifications.push(Verification::new_table(
                                this_table, table, src, None,
                            ))
                        }
                        _ => return Err(PatternError::InvalidRef(src)),
                    }
                }

                block::pattern::Field::SubPattern(sub) => {
                    let pattern =
                        Pattern::new(sleigh, sub.clone(), this_table)?;
                    //add/verify all token fields
                    pattern
                        .token_fields
                        .values()
                        .map(|prod| {
                            let mut prod = prod.clone();
                            prod.local = false;
                            prod
                        })
                        .try_for_each(&mut add_explicit_token_field)?;
                    verifications.push(Verification::SubPattern {
                        //TODO improve this src
                        location: sub.src.clone(),
                        pattern,
                    })
                }
            }
        }

        //tokens (that can produce implicit fields) are only taken from local,
        //sub_patterns can't produce implicit token_fields
        let fields_from_verifications =
            verifications
                .iter()
                .filter_map(
                    |verification: &Verification| match &verification {
                        Verification::TokenFieldCheck { field, .. } => {
                            Some(field.element())
                        }
                        Verification::ContextCheck { .. }
                        | Verification::TableBuild { .. }
                        | Verification::SubPattern { .. } => None,
                    },
                );
        let fields_from_explicit_fields = token_fields
            .values()
            .map(|token_fields| token_fields.field.element());
        let token_from_pattern = fields_from_explicit_fields
            .chain(fields_from_verifications)
            .map(|field| field.token.reference());
        let mut tokens = IndexMap::new();
        for token in token_from_pattern {
            tokens
                .entry(token.element_ptr())
                .and_modify(|(num, _token)| {
                    *num += 1;
                })
                .or_insert_with(|| (1, token));
        }

        let tables = verifications
            .iter()
            .filter_map(Verification::tables)
            .flatten()
            .map(|produced_table| {
                (produced_table.table.element_ptr(), produced_table.clone())
            })
            .collect();

        Ok(Self {
            op: Op::And,
            location,
            len: None,
            token_fields,
            tokens,
            verifications,
            tables,
            pre: vec![],
            pos: vec![],
        })
    }
    pub fn new(
        sleigh: &Sleigh,
        input: block::pattern::Block,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        let block::pattern::Block {
            src,
            first,
            elements,
        } = input;

        //NOTE I'll not allow to mix `&` and `|` in the same level
        let op = match &elements[..] {
            //a single element don't care for operations, just default to and
            [] => block::pattern::Op::And,
            [(first_op, _), rest @ ..] => {
                if rest.iter().all(|(op, _)| first_op == op) {
                    *first_op
                } else {
                    return Err(PatternError::MixOperations(src.clone()));
                }
            }
        };

        let all_elements = [first]
            .into_iter()
            .chain(elements.into_iter().map(|(_op, element)| element));

        match op {
            block::pattern::Op::Or => {
                Self::new_or(sleigh, src, all_elements, this_table)
            }
            block::pattern::Op::And => {
                Self::new_and(sleigh, src, all_elements, this_table)
            }
        }
    }
    //token fields required by value comparisons that this block can't produce
    //itself
    pub fn unresolved_token_fields(
        &self,
    ) -> IndexMap<*const TokenField, GlobalReference<TokenField>> {
        let mut this_unresolved = FindValues::search(self.verifications.iter());
        //remove all token_fields that can be produced locally
        this_unresolved.retain(|_key, token_field| {
            let token_field = token_field.element();
            let token = &token_field.token;
            !self.tokens.contains_key(&token.element_ptr())
        });
        //for each sub_pattern, find unresolved token_fields
        this_unresolved.extend(
            self.verifications
                .iter()
                .map(Verification::sub_pattern)
                .flatten()
                .map(Pattern::unresolved_token_fields)
                .flatten(),
        );
        ////remove all token_fields that is produced locally
        //this_unresolved.retain(|_key, token_field| {
        //    !self.token_fields.contains_key(&token_field.element_ptr())
        //});
        this_unresolved
    }
    pub fn is_token_field_produced(
        &self,
        search_field: &GlobalReference<TokenField>,
    ) -> bool {
        self.token_fields.get(&search_field.element_ptr()).is_some()
    }
    pub fn is_table_produced(
        &self,
        token_field: &GlobalReference<Table>,
    ) -> bool {
        self.tables.get(&token_field.element_ptr()).is_some()
    }
    pub fn produce_token_field(
        &mut self,
        token_field: &GlobalReference<TokenField>,
    ) -> bool {
        if self.is_token_field_produced(token_field) {
            return true;
        }
        let token_field_element = token_field.element();
        let token = &token_field_element.token;
        let token_num_used = self
            .tokens
            .get(&token.element_ptr())
            .map(|(num, _)| *num)
            .unwrap_or(0);
        if token_num_used == 1 {
            self.token_fields.insert(
                token_field.element_ptr(),
                ProducedTokenField {
                    explicit: false,
                    field: token_field.clone(),
                    local: true,
                },
            );
            true
        } else {
            false
        }
    }
    pub fn solve_or<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        //each branch of the or is represented by each verification
        enum OrLenPossible {
            Recursive, //recursive in `OR` is not allowed
            Unknown,   //at least one branch len is not known
        }
        let mut branch_len_iter = self
            .verifications
            .iter()
            .map(Verification::pattern_len)
            .map(|len| match len {
                Some(ConstructorPatternLen::Basic(len)) => Ok(len),
                Some(
                    ConstructorPatternLen::NonGrowingRecursive(_)
                    | ConstructorPatternLen::GrowingRecursive { .. },
                ) => Err(OrLenPossible::Recursive),
                None => Err(OrLenPossible::Unknown),
            });
        //unwrap never happen because empty pattern default to `Op::And`
        let first = branch_len_iter.next().unwrap();
        let new_len = first.and_then(|first| {
            branch_len_iter.try_fold(first, |acc, x| Ok(acc.intersection(x?)))
        });
        match new_len {
            //all the lens are valid
            Ok(new_len) => {
                solved.i_did_a_thing();
                self.len = Some(ConstructorPatternLen::Basic(new_len));
            }
            //at least one len is not known yet, request another
            //run to solve it
            Err(OrLenPossible::Unknown) => {
                solved.iam_not_finished_location(
                    &self.location,
                    file!(),
                    line!(),
                );
            }
            //at least one len is recursive
            Err(OrLenPossible::Recursive) => {
                return Err(PatternError::InvalidOrLen(self.location.clone()))
            }
        }
        Ok(())
    }
    pub fn solve_and<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        //the pattern len is the biggest of all tokens, all sub_patterns
        //and all tables in the pattern
        let tokens_len = self
            .tokens
            .values()
            .map(|(_num, token)| token.element().len_bytes())
            .max()
            .map(|token_len| {
                ConstructorPatternLen::Basic(PatternLen::Defined(
                    token_len.get(),
                ))
            });
        let mut verifications_len =
            self.verifications.iter().map(Verification::pattern_len);
        let first = tokens_len.unwrap_or(PatternLen::Defined(0).into());
        let final_len =
            verifications_len.try_fold(first, |acc, x| acc.greater(x?));

        if let Some(final_len) = final_len {
            //basic means fully solved
            if !final_len.is_basic() {
                //need to solve the recursive len of the pattern
                solved.iam_not_finished_location(
                    &self.location,
                    file!(),
                    line!(),
                );
            }
            if self.len != Some(final_len) {
                solved.i_did_a_thing();
                self.len = Some(final_len);
            }
        } else {
            //if not fully finished yet, request another run
            solved.iam_not_finished_location(&self.location, file!(), line!());
        }
        Ok(())
    }
    pub fn solve<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        //if len is already solved and no unresolved_token_field, there is
        //nothing todo
        if is_len_finished(&self.len) {
            return Ok(());
        }
        //call solve in all sub_patterns
        self.verifications
            .iter_mut()
            .filter_map(Verification::sub_pattern_mut)
            .map(|sub| sub.solve(solved))
            .collect::<Result<_, _>>()?;
        //TODO check all table value verifications export a const value
        match self.op {
            Op::And => self.solve_and(solved)?,
            Op::Or => self.solve_or(solved)?,
        }
        if !is_len_finished(&self.len) {
            solved.iam_not_finished_location(&self.location, file!(), line!());
        }
        Ok(())
    }
    pub fn root_len(&self) -> usize {
        match self.op {
            Op::And => {
                //get the biggest token, from all token_field verifications and
                //explicit/implicit token_fields
                let biggest_token_bytes = self
                    .tokens
                    .values()
                    .map(|(_num, token)| token.element().len_bytes().get())
                    .max()
                    .unwrap_or(0);
                let biggest_token =
                    usize::try_from(biggest_token_bytes).unwrap() * 8usize;
                //calc the len of the biggest sub_pattern
                let biggest_sub_pattern = self
                    .verifications
                    .iter()
                    .filter_map(Verification::sub_pattern)
                    .map(Pattern::root_len)
                    .max()
                    .unwrap_or(0);
                //return the biggest one
                biggest_token.max(biggest_sub_pattern)
            }
            Op::Or => {
                //get the biggest of all branches
                self.verifications
                    .iter()
                    .map(Verification::root_len)
                    .max()
                    .unwrap_or(0)
            }
        }
    }
    fn constrait_same_field(&self, constraint: &mut [BitConstraint]) -> bool {
        //if all the verifications are using the same field, we can just
        //OR it
        let mut iter = self
            .verifications
            .iter()
            .map(Verification::token_field_check)
            .flatten();
        let Some(first) = iter.next() else {
            return false;
        };

        if iter.any(|this| first != this) {
            return false;
        }
        //or all the branches into this pattern, first element forms the base
        let mut verifications = self.verifications.iter();
        let first = verifications.next().unwrap();
        let mut out_buf = vec![BitConstraint::Unrestrained; constraint.len()];
        first.constraint_variant(&mut out_buf);

        //apply all other verification
        let mut branch_buf =
            vec![BitConstraint::Unrestrained; constraint.len()];
        for verification in verifications {
            branch_buf
                .iter_mut()
                .for_each(|bit| *bit = BitConstraint::Unrestrained);
            verification.constraint_variant(&mut branch_buf);
            out_buf.iter_mut().zip(branch_buf.iter()).for_each(
                |(out, branch)| {
                    *out = out.least_restrictive(*branch);
                },
            );
        }
        //or it all up with the original constraint
        constraint.iter_mut().zip(out_buf.iter()).for_each(
            |(constraint, out)| {
                *constraint = constraint.most_restrictive(*out);
            },
        );
        true
    }
    pub fn constraint(&self, constraint: &mut BlockConstraint, offset: usize) {
        if self.op == Op::And {
            self.verifications
                .iter()
                .for_each(|ele| ele.constraint(constraint, offset))
        } else {
            //if all the verifications are using the same field, we can just
            //OR it
            if self.constrait_same_field(&mut constraint.base[offset..]) {
                return;
            }

            //mark as being used, so we fail if we recurse
            if constraint.variants_lock {
                //TODO make this an error
                panic!("Recursives Or pattern is not allowed")
            }
            constraint.variants_lock = true;
            let old_variants = constraint.variants.take();

            let new_variants: Vec<_> = self
                .verifications
                .iter()
                .map(|verification| {
                    let mut variant =
                        vec![BitConstraint::default(); constraint.base.len()];
                    verification.constraint_variant(&mut variant[offset..]);
                    variant
                })
                .collect();
            if let Some(old_variants) = old_variants {
                // Limite the use of Or to a reasanable number, make this an
                // error?
                let combined_len = old_variants.len() * new_variants.len();
                if combined_len > 1024 {
                    todo!("Pattern is too big");
                }
                //combine all the old variants with the new ones
                let mut combined_variants = Vec::with_capacity(combined_len);
                for old_variant in old_variants.iter() {
                    for new_variant in new_variants.iter() {
                        let combine_variant = old_variant
                            .iter()
                            .zip(new_variant.iter())
                            .map(|(old, new)| old.most_restrictive(*new))
                            .collect();
                        combined_variants.push(combine_variant);
                    }
                }
                constraint.variants = Some(combined_variants);
            } else {
                constraint.variants = Some(new_variants);
            }
            constraint.variants_lock = false;
            //TODO check and filter out impossible variants
        }
    }
    pub fn constraint_variant(&self, constraint: &mut [BitConstraint]) {
        if self.op == Op::And {
            self.verifications
                .iter()
                .for_each(|ele| ele.constraint_variant(constraint))
        } else {
            // this level of or is only allowed if we have multiple values for
            // the save field
            if !self.constrait_same_field(constraint) {
                //TODO make this an error
                dbg!(&self);
                todo!("Or Inside Or")
            }
        }
    }
}
impl From<Block> for FinalBlock {
    fn from(value: Block) -> Self {
        let exported_token_fields =
            value.token_fields.values().map(|prod| &prod.field);
        let verified_token_fields =
            value.verifications.iter().filter_map(|ver| match ver {
                Verification::ContextCheck { .. }
                | Verification::TableBuild { .. }
                | Verification::SubPattern { .. } => None,
                Verification::TokenFieldCheck {
                    field,
                    op: _,
                    value: _,
                } => Some(field),
            });
        let token_len = exported_token_fields
            .chain(verified_token_fields)
            .map(|token_field| token_field.element().token.len_bytes().get())
            .max();
        let token_fields = value
            .token_fields
            .into_iter()
            .map(|(_, k)| k.into())
            .collect();
        let tables = value.tables.into_iter().map(|(_, k)| k.into()).collect();
        let verifications = value
            .verifications
            .into_iter()
            .map(|k| k.try_into().unwrap())
            .collect();
        match value.op {
            Op::And => FinalBlock::And {
                len: value.len.unwrap().basic().unwrap(),
                token_len: token_len.unwrap_or(0),
                token_fields,
                tables,
                verifications,
                pre: value.pre.into_iter().map(|x| x.convert()).collect(),
                pos: value.pos.into_iter().map(|x| x.convert()).collect(),
            },
            Op::Or => FinalBlock::Or {
                len: value.len.unwrap().basic().unwrap(),
                token_fields,
                tables,
                branches: verifications,
                pos: value
                    .pre
                    .into_iter()
                    .chain(value.pos.into_iter())
                    .map(|x| x.convert())
                    .collect(),
            },
        }
    }
}
