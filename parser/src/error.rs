use tokenizer::Bracket;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    ExpectedExpr,
    ExpectedIdent,
    ExpectedTerminator,
    ExpectedComma,
    ExpectedAssignment,
    ExpectedItemDeclaration,
    ExpectedAtleastType,

    // bracket
    ExpectedOpenParen,
    ExpectedClosedBracket { opened: Bracket },
    LonelyClosedBracket { closed: Bracket },
}
