use logos::{Logos, Lexer};
use std::rc::Rc;

use crate::node::*;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(";")]
    Semicolon,
    
    // keywords
    #[token("int")]
    Int,
    #[token("return")]
    Ret,

    #[regex(r"[a-zA-Z]\w*", |lex| format!("{}", lex.slice()))]
    Ident(String),
    #[regex(r"[0-9]+", parse_int)]
    IntLiteral(i64),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("expected a semicolon")]
    ExpectedSemicolon,
    #[error("expected an identifier")]
    ExpectedIdent,
}

// TODO: implement parsing for hex and binary numbers
// (the `0xXXXXXXXX` and `0bXXXXXXXX` formats)
fn parse_int(lex: &mut Lexer<Token>) -> Option<i64> {
    lex.slice().parse().ok()
}

/// Returns the next `Node` that is 1 level below the scope it was called from,
/// advancing the cursor in the process.
fn parse_next(cursor: &mut TokenCursor) -> anyhow::Result<Node, ParseError> {
    match cursor.next() {
        Some(Token::Ret) => {
            let rv = parse_next(cursor)?;
            Ok(Node::Return(Rc::new(rv)))
        }
        // literals
        Some(Token::IntLiteral(val)) =>
            match cursor.next() {
                Some(Token::Semicolon) => Ok(Node::Constant(Val::I64(val))),
                _ => Err(ParseError::ExpectedSemicolon),
            }
        Some(Token::Int) => 
            if let Some(Token::Ident(name)) = cursor.next() {
                // FIXME: for now, we'll just skip over 2 tokens to skip the parens
                cursor.next(); cursor.next();
                match cursor.next() {
                    Some(Token::Semicolon) => Ok(empty_func_decl(name, "int".into())),
                    Some(Token::LBrace) => {
                        let mut children = Vec::new();
                        loop {
                            if let Token::RBrace = cursor.peek() {
                                return Ok(Node::FuncDecl {
                                    name,
                                    ret_type: "int".to_string(),
                                    children
                                })
                            }

                            children.push(parse_next(cursor)?);
                        }
                    }
                    _ => Err(ParseError::ExpectedSemicolon),
                }
            } else { Err(ParseError::ExpectedIdent) }
        _ => todo!()
    }
}

struct TokenCursor {
    tokens: Vec<Token>,
    pos: usize,
}
impl Iterator for TokenCursor {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.tokens.len() { return None }
        let res = self.tokens[self.pos].clone();
        self.pos += 1;
        Some(res)
    }
}
impl TokenCursor {
    fn prev(&mut self) -> Option<Token> {
        if self.pos == 0 { return None }
        self.pos -= 1;
        Some(self.tokens[self.pos].clone())
    }

    fn peek(&self) -> Token {
        // SAFETY: since `pos` is not allowed to increment or decrement out of bounds,
        // indexing into `tokens` is always safe and will always yield a result.
        self.tokens[self.pos].clone()
    }

    fn from(buf: &[Token]) -> Self {
        Self {
            tokens: Vec::from(buf),
            pos: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    const RETURN2: &'static str = r"
int main() {
    return 2;
}
";

    #[test]
    fn test_lex_return2_program() {
        let mut lex = Token::lexer(RETURN2);
        // int
        assert_eq!(lex.next(), Some(Token::Int));
        // main
        assert_eq!(lex.next(), Some(Token::Ident(String::from("main"))));
        // ()
        assert_eq!(lex.next(), Some(Token::LParen));
        assert_eq!(lex.next(), Some(Token::RParen));
        // {
        assert_eq!(lex.next(), Some(Token::LBrace));
        // return
        assert_eq!(lex.next(), Some(Token::Ret));
        // 2
        assert_eq!(lex.next(), Some(Token::IntLiteral(2)));
        // ;
        assert_eq!(lex.next(), Some(Token::Semicolon));
        // }
        assert_eq!(lex.next(), Some(Token::RBrace));
    }

    #[test]
    fn test_parse_return2_statement() {
        let input  = "return 2;";
        let tokens = Token::lexer(input).collect::<Vec<_>>();

        let node = parse_next(&mut TokenCursor::from(&tokens)).unwrap();
        match node {
            Node::Return(rc) => assert_eq!(*rc, Node::Constant(Val::I64(2))),
            _ => panic!("`node` == {:?}", node),
        }
    }

    #[test]
    fn test_parse_int_main_decl() {
        let input = "int main();";
        let tokens = Token::lexer(input).collect::<Vec<_>>();

        let node = parse_next(&mut TokenCursor::from(&tokens)).unwrap();
        match node {
            Node::FuncDecl { name, ret_type, children } => {
                assert_eq!(name, "main".to_string());
                assert_eq!(ret_type, "int".to_string());
                assert_eq!(children, Vec::new());
            }
            _ => panic!("`node` == {:?}", node),
        }
    }

    #[test]
    fn test_parse_int_main_def() {
       let input = "int main() {}";
       let tokens = Token::lexer(input).collect::<Vec<_>>();

       let node = parse_next(&mut TokenCursor::from(&tokens)).unwrap();
       match node {
           Node::FuncDecl { name, ret_type, children } => {
               assert_eq!(name, "main".to_string());
               assert_eq!(ret_type, "int".to_string());
               assert_eq!(children, Vec::new());
           }
           _ => panic!("`node` == {:?}", node),
       }
    }
    
    #[test]
    fn test_parse_return2_program() {
        let tokens = Token::lexer(RETURN2).collect::<Vec<_>>();
        let node   = parse_next(&mut TokenCursor::from(&tokens)).unwrap();
        match node {
            Node::FuncDecl { name, ret_type, children } => {
                assert_eq!(name, "main".to_string());
                assert_eq!(ret_type, "int".to_string());
                assert!(children.len() == 1);

                match &children[0] {
                    Node::Return(rc) => assert_eq!(**rc, Node::Constant(Val::I64(2))),
                    _ => panic!("`children` == {:?}", children),
                }
            }
            _ => panic!("`node` == {:?}", node),
        }
    }
}
