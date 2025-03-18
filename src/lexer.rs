/// Lexer
pub struct Lexer {
    // TODO: lines should be an iterator over Strings, not an owned Vec
    lines: Vec<String>,
}

impl Lexer {
    pub fn lex(lines: Vec<String>) -> anyhow::Result<Vec<Token>> {
        let lexer = Lexer { lines };
        let mut tokens: Vec<Token> = vec![];

        // TODO: row is for location, see Loc
        'line: for (row, line) in lexer.lines.iter().enumerate() {
            let _ = row;

            let mut chars = line.char_indices().peekable();
            'chars: while let Some((start, _)) = chars.next() {
                for (symbol, token) in Token::STRING_TOKEN_MAP {
                    if line[start..].starts_with(symbol) {
                        // Skip comments no matter where they show up!
                        // TODO: Add Doc comment support
                        if token == &Token::Comment || token == &Token::DocComment {
                            continue 'line;
                        }

                        tokens.push(token.clone());

                        if symbol.len() > 1 {
                            // chars.next();
                            chars.nth(symbol.len() - 2);
                        }

                        continue 'chars;
                    }
                }

                let ch = line.chars().nth(start).unwrap();
                if ch.is_alphabetic() {
                    let indent: String = line[start..]
                        .chars()
                        .take_while(|c| c.is_alphabetic() || *c == '_')
                        .collect();
                    if indent.len() > 1 {
                        // chars.next();
                        chars.nth(indent.len() - 2);
                    }
                    tokens.push(Token::Ident(indent));
                    continue 'chars;
                } else if ch.is_ascii_digit() {
                    let number: String = line[start..]
                        .chars()
                        .take_while(|c| c.is_ascii_digit())
                        .collect();
                    if number.len() > 1 {
                        // chars.next();
                        chars.nth(number.len() - 2);
                    }
                    tokens.push(Token::Number(number));
                    continue 'chars;
                } else {
                    if ch.is_whitespace() {
                        continue 'chars;
                    }
                    tokens.push(Token::Unknown(ch.to_string()));
                    continue 'chars;
                }
            }
        }

        Ok(tokens)
    }
}

/// Location of the token
/// TODO:
/// - This is niave and can be calculated later on in the pipeline
/// - Implement Loc into Token, I just didn't want to do that yet (I'm lazy)
pub enum Loc {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Unknown sized tokens
    Ident(String),
    Number(String),
    Float(String),
    Unknown(String),

    Plus,         // `+`
    Minus,        // `-`
    Star,         // `*`
    ForwardSlash, // `/`
    BackSlash,    // `\`
    Colon,        // `:`
    SemiColon,    // `;`
    Bang,         // `!`
    At,           // `@
    Hash,         // `#`
    Dollar,       // `$`
    Percent,      // `%`
    Caret,        // `^`
    Ampersand,    // `&`
    LeftParen,    // `(
    RightParen,   // `)`
    LeftBrace,    // `{`
    RightBrace,   // `}`
    LeftBracket,  // `[`
    RightBracket, // `]`
    SingleQuote,  // `'`
    DoubleQuote,  // `"`
    LessThan,     // `<`
    GreaterThan,  // `>`
    Period,       // `.`
    Comma,        // `,
    Question,     // `?`
    PipeLine,     // `|`

    // Double char tokens
    Comment,       // `//`
    PlusPlus,      // `++`
    MinusMinus,    // `--`
    Pow,           // ^^
    Pipe,          // `|>`
    DoubleColon,   // `::`
    DoubleEq,      // `==`
    NotEq,         // `!=`
    LessEq,        // `<=`
    GreaterEq,     // `>=`
    LeftArrow,     // `<-`
    RightArrow,    // `->`
    FatRightArrow, // `=>`

    // Triple char tokens
    ShiftLeftEq,  // `<<=`
    ShiftRightEq, // `>>=`
    DocComment,   // `//!`
}

impl Token {
    // Must be stored from longest (3 char) to shortest (1 char) for correct lexing
    const STRING_TOKEN_MAP: &'static [(&'static str, Token)] = &[
        // Three char tokens
        ("<<=", Self::ShiftLeftEq),
        (">>=", Self::ShiftRightEq),
        ("//!", Self::DocComment),
        // Two char tokens
        ("//", Self::Comment),
        ("++", Self::PlusPlus),
        ("--", Self::MinusMinus),
        ("^^", Self::Pow),
        ("|>", Self::Pipe),
        ("::", Self::DoubleColon),
        ("==", Self::DoubleEq),
        ("!=", Self::NotEq),
        ("<=", Self::LessEq),
        (">=", Self::GreaterEq),
        ("->", Self::RightArrow),
        ("<-", Self::LeftArrow),
        ("=>", Self::FatRightArrow),
        // One char tokens
        ("+", Self::Plus),
        ("-", Self::Minus),
        ("*", Self::Star),
        ("/", Self::ForwardSlash),
        ("\\", Self::BackSlash),
        (":", Self::Colon),
        (";", Self::SemiColon),
        ("!", Self::Bang),
        ("@", Self::At),
        ("#", Self::Hash),
        ("$", Self::Dollar),
        ("%", Self::Percent),
        ("^", Self::Caret),
        ("&", Self::Ampersand),
        ("(", Self::LeftParen),
        (")", Self::RightParen),
        ("{", Self::LeftBrace),
        ("}", Self::RightBrace),
        ("[", Self::LeftBracket),
        ("]", Self::RightBracket),
        ("'", Self::SingleQuote),
        ("\"", Self::DoubleQuote),
        ("<", Self::LessThan),
        (">", Self::GreaterThan),
        (".", Self::Period),
        (",", Self::Comma),
        ("?", Self::Question),
        ("|", Self::PipeLine),
    ];

    pub fn string_to_token(s: &str) -> Option<Self> {
        Self::STRING_TOKEN_MAP.iter().find_map(
            |(key, value)| {
                if *key == s { Some(value.clone()) } else { None }
            },
        )
    }
}
