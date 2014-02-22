use parser::{Parser, ParseResult, Success, Failure};
use state::{State, StrState};

#[deriving(Eq, ToStr, Clone)]
pub struct Char {
    priv c: char
}

impl Parser<char, StrState> for Char {
  fn run(&self, input: StrState) -> ParseResult<char, StrState> {
    match input.head() {
      Some((head, inp)) =>
        if head == self.c { Success(self.c, inp) }
        else { Failure(format!("Could not find matching char. found: {:?} required: {:?}", head, self.c)) },
      None => Failure(format!("Could not match char {:?} because EOI.", self.c)),
    }
  }
}

pub fn chr(c: char) -> Char {
  Char{ c: c }
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
        else { Failure(format!("Could not find matching string. found: {:?} required: {:?}", t, self.txt)) },
      None => Failure(format!("Could not match string {:?} because EOI.", self.txt)),
    }
  }
}

pub fn text(txt: ~str) -> Text {
  Text{ txt: txt }
}

#[deriving(Eq, ToStr, Clone)]
pub struct AnyOfChar {
  priv valid: ~str
}

impl Parser<char, StrState> for AnyOfChar {
  fn run(&self, input: StrState) -> ParseResult<char, StrState> {
    match input.head() {
      Some((ref c, ref inp)) if self.valid.contains_char(*c) => Success(*c, inp.clone()),
      Some((ref c, _)) => Failure(format!("Could not find matching char. found: {:?} required any of: {:?}", *c, self.valid)),
      None => Failure(format!("Could not match any of {:?} because EOI.", self.valid)),
    }
  }
}

pub fn anyOfChar(valid: ~str) -> AnyOfChar {
  AnyOfChar{ valid: valid }
}

#[cfg(test)]
mod tests {
  use parser::{Parser};
  use parser::string::{chr, text, anyOfChar};
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

    assert_eq!(failure_nomatch.err(), ~"Could not find matching char. found: 'H' required: 'b'");

    let failure_eof = chr('a').run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match char 'a' because EOI.");
  }

  #[test]
  fn test_str() {
    let (txt, rest) = text(~"Hell").run(input()).unwrap();

    assert_eq!(txt, ~"Hell");
    assert_eq!(rest.content(), ~"o, 世界");

    let failure_nomatch = text(~"af").run(input());

    assert_eq!(failure_nomatch.err(), ~"Could not find matching string. found: ~\"He\" required: ~\"af\"");

    let failure_eof = text(~"ab").run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match string ~\"ab\" because EOI.");
  }

  #[test]
  fn test_any_of_char(){
    let (c, rest) = anyOfChar(~"Hb").run(input()).unwrap();

    assert_eq!(c, 'H');
    assert_eq!(rest.content(), ~"ello, 世界");

    let failure_nomatch = anyOfChar(~"ab").run(StrState::new(~"def"));

    assert_eq!(failure_nomatch.err(), ~"Could not find matching char. found: 'd' required any of: ~\"ab\"");

    let failure_eof = anyOfChar(~"ab").run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match any of ~\"ab\" because EOI.");
  }
}
