use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,
    Keyword(Keywords),
    Ident(String),
    Int(u64),
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keywords {
    Int,
    Return
}

/// Returns the next token and advances the iterator past it.
pub fn next_token(iter: &mut dyn Iterator<Item=char>) -> anyhow::Result<Token> {
    use Token::*;
    
    match iter.next() {
        None      => Ok(EOF),
        Some('{') => Ok(LBrace),
        Some('}') => Ok(RBrace),
        Some('(') => Ok(LParen),
        Some(')') => Ok(RParen),
        Some(';') => Ok(Semicolon),
        
        Some(ch) if ch.is_whitespace() => next_token(iter),
        Some(ch) if ch.is_numeric() => {
            let strval = String::from(ch).chars().chain(
                    iter.take_while(|ch| ch.is_numeric())
                ).collect::<String>();

            Ok(Int(u64::from_str(&strval)?))
        }
        Some(ch) if ch.is_alphabetic() || ch == '_' => {
            let strval = String::from(ch).chars().chain(
                    iter.take_while(|ch| ch.is_alphabetic()
                                    || ch.is_numeric()
                                    || *ch == '_')
                ).collect::<String>();
            
            match strval.as_str() {
                "int"    => Ok(Keyword(Keywords::Int)),
                "return" => Ok(Keyword(Keywords::Return)),
                _        => Ok(Ident(strval))
            }
        }
        Some(ch) => anyhow::bail!("Invalid: {}{}", ch, iter.collect::<String>())
    }
}

/// Consumes the iterator and returns a `Vec` of tokens.
pub fn lex(iter: &mut dyn Iterator<Item=char>) -> anyhow::Result<Vec<Token>> {
    let mut acc = Vec::new();

    loop {
        let new_tok = next_token(iter)?;
        if new_tok == Token::EOF {
            return Ok(acc);
        }
        acc.push(new_tok);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_brackets_parens() {
        let mut input    = String::new();
        let mut expected = Vec::new();

        // get a 50 char test case
        for _ in 0..50 {
            if rand::random::<bool>() {
                input.push('(');
                expected.push(Token::LParen);
            } else {
                input.push(')');
                expected.push(Token::RParen);
            }
            if rand::random::<bool>() {
                input.push('{');
                expected.push(Token::LBrace);
            } else {
                input.push('}');
                expected.push(Token::RBrace);
            }
        }

        let got = lex(&mut input.chars()).unwrap();
        assert_eq!(expected, got, "Input: {}\nResult: {:?}", input, got);
    }

    #[test]
    fn test_keywords_int_return() {
        let mut input    = String::new();
        let mut expected = Vec::new();

        // generate a 50 token test case
        for _ in 0..50 {
            if rand::random::<bool>() {
                input = format!("{}{} ", input, "int");
                expected.push(Token::Keyword(Keywords::Int));
            } else {
                input = format!("{}{} ", input, "return");
                expected.push(Token::Keyword(Keywords::Return));
            }
        }

        let got = lex(&mut input.chars()).unwrap();
        assert_eq!(expected, got, "Input: {}\nResult: {:?}", input, got);
    }

    #[test]
    fn test_int_literals() {
        let mut input    = String::new();
        let mut expected = Vec::new();

        // generate a 50 token test case
        for _ in 0..50 {
            let int = rand::random::<u64>();
            input = format!("{}{} ", input, int);
            expected.push(Token::Int(int));
        }

        let got = lex(&mut input.chars()).unwrap();
        assert_eq!(expected, got, "Input: {}\nResult: {:?}", input, got);
    }

    #[test]
    fn test_ident() {
        let input    = "dshldsahdk __hidadiushdui_9238";
        let expected = vec![
            Token::Ident(String::from("dshldsahdk")),
            Token::Ident(String::from("__hidadiushdui_9238")),
        ];
        let got = lex(&mut input.chars()).unwrap();
        assert_eq!(expected, got);
    }
}
