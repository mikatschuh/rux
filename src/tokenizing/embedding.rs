use crate::{
    error::{Errors, Position},
    tokenizing::{parse_tok::parse_quote, token::Token},
};

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct EmbeddingSyntax {
    open_braces_after_embedding_quote: Vec<usize>,
}

impl EmbeddingSyntax {
    pub fn embedded_scope_opening(&mut self) {
        self.open_braces_after_embedding_quote.push(0)
    }

    pub fn opening_curly_brace<'src>(&mut self) {
        self.open_braces_after_embedding_quote
            .last_mut()
            .map(|open_braces_after_embedding_quote| *open_braces_after_embedding_quote += 1);
    }

    pub fn closing_curly_brace<'src>(
        &mut self,
        text: &mut &'src [u8],
        pos: Position,
        errors: &mut Errors,
    ) -> Option<(Token<'src>, String)> {
        if let Some(open_braces_after_embedding_quote) =
            self.open_braces_after_embedding_quote.last_mut()
        {
            if *open_braces_after_embedding_quote == 0 {
                self.open_braces_after_embedding_quote.pop();

                return Some(parse_quote(text, pos, self, true, errors));
            }
            *open_braces_after_embedding_quote -= 1;
        }
        None
    }
}
