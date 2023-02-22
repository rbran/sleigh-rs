use crate::Span;
use crate::preprocessor::DisplayToken;
use crate::preprocessor::FilePreProcessor;
use crate::SleighError;

#[derive(Clone, Debug)]
pub enum DisplayElement {
    Ident(Span, String),
    Literal(Span, String),
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
                DisplayToken::Concat => (),
                DisplayToken::Ident(src, ident) => {
                    display.push(DisplayElement::Ident(src, ident))
                }
                DisplayToken::Literal(src, lit) => {
                    display.push(DisplayElement::Literal(src, lit))
                }
            }
        }
        Ok(Self(display))
    }
}
