use parser::{Map, Parser, ParseResult, Success, Failure};
use parser::generic::{one, One};
use state::{StrState};
use std::str::from_chars;

mod generic;

pub trait ConcatenableParser {
  fn concat(&self) -> Map<Self, ~[char], ~str>;
}

impl<A: Parser<~[char], StrState>> ConcatenableParser for A {
  fn concat(&self) -> Map<A, ~[char], ~str> {
    fn cat(input: ~[char]) -> ~str { from_chars(input) }
    self.map(cat)
  }
}

type Chr = One<char, StrState>;

pub fn chr(c: char) -> Chr {
  one(c)
}

#[deriving(Eq, ToStr, Clone)]
pub struct Text {
  priv txt: ~str
}

impl Parser<~str, StrState> for Text {
  fn run(&self, input: StrState) -> ParseResult<~str, StrState> {
    match input.take(self.txt.char_len()) {
      Some((t, inp)) =>
        if t == self.txt { Success(self.txt.clone(), inp) }
        else { Failure(format!("Could not find matching string. Found: {:?} required: {:?}", t, self.txt)) },
      None => Failure(format!("Could not match string {:?} because EOI.", self.txt)),
    }
  }
}

pub fn text(txt: ~str) -> Text {
  Text{ txt: txt }
}

type AnyOf = generic::AnyOf<char, StrState>;

pub fn anyOf(valid: ~str) -> AnyOf {
  return generic::anyOf(valid.chars().to_owned_vec())
}

#[cfg(test)]
mod tests {
  use parser::{Parser};
  use parser::string::{chr, text, anyOf};
  use state::{StrState};

  fn input() -> StrState {
     StrState::new(~"Hello, 世界")
  }

  #[test]
  fn test_chr() {
    let (head, rest) = chr('H').run(input()).unwrap();

    assert_eq!(head, 'H');
    assert_eq!(rest.content(), ~"ello, 世界");

    let failure_nomatch = chr('b').run(input());

    assert_eq!(failure_nomatch.err(), ~"Could not find matching element. Found: 'H' required: 'b'");

    let failure_eof = chr('a').run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match char 'a' because EOI.");
  }

  #[test]
  fn test_str() {
    let (txt, rest) = text(~"Hell").run(input()).unwrap();

    assert_eq!(txt, ~"Hell");
    assert_eq!(rest.content(), ~"o, 世界");

    let failure_nomatch = text(~"af").run(input());

    assert_eq!(failure_nomatch.err(), ~"Could not find matching string. Found: ~\"He\" required: ~\"af\"");

    let failure_eof = text(~"ab").run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match string ~\"ab\" because EOI.");
  }

  #[test]
  fn test_any_of_char(){
    let (c, rest) = anyOf(~"Hb").run(input()).unwrap();

    assert_eq!(c, 'H');
    assert_eq!(rest.content(), ~"ello, 世界");

    let failure_nomatch = anyOf(~"ab").run(StrState::new(~"def"));

    assert_eq!(failure_nomatch.err(), ~"Could not find matching element. Found: 'd' required any of: ~['a', 'b']");

    let failure_eof = anyOf(~"ab").run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match any of ~['a', 'b'] because EOI.");
  }
}
