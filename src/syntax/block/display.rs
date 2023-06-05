use crate::Span;
use crate::preprocessor::DisplayToken;
use crate::preprocessor::FilePreProcessor;
use crate::SleighError;

#[derive(Clone, Debug)]
pub enum DisplayElement {
    Concat,
    Ident(Span, String),
    Literal(String),
    Other(char),
}

#[derive(Clone, Debug)]
pub struct Display(pub Vec<DisplayElement>);

impl Display {
    pub fn parse(input: &mut FilePreProcessor) -> Result<Self, SleighError> {
        let mut display = vec![];
        loop {
            let token = input.parse_display()?;
            match token {
                DisplayToken::End => break,
                DisplayToken::Concat => display.push(DisplayElement::Concat),
                DisplayToken::Ident(src, ident) => {
                    display.push(DisplayElement::Ident(src, ident))
                }
                DisplayToken::Literal(lit) => {
                    display.push(DisplayElement::Literal(lit))
                }
                DisplayToken::Other(c) => {
                    display.push(DisplayElement::Other(c))
                }
            }
        }
        Ok(Self(display))
    }
}
