use parser::{Parser, ParseResult, Success, Failure};
use state::{State, StrState};

mod parser;
mod state;

#[deriving(Eq, ToStr, Clone)]
pub struct Accept<A> {
  priv res: A
}

impl<A: Clone, T, S: State<T>> Parser<A, S> for Accept<A> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    Success(self.res.clone(), input)
  }
}

pub fn success<A: Clone>(result: A) -> Accept<A> {
  Accept{ res: result }
}

#[deriving(Eq, ToStr, Clone)]
pub struct Peek;

impl<A: Clone, S: State<A>> Parser<A, S> for Peek {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match input.head() {
      Some((c, inp)) => Success(c, inp),
      None => Failure(format!("Could not match char because EOI.")),
    }
  }
}

pub fn peek() -> Peek {
  Peek
}

#[deriving(Eq, ToStr, Clone)]
pub struct Elem<A> {
  priv e: A
}

impl<A: Clone + Eq, S: State<A>> Parser<A, S> for Elem<A> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match input.head() {
      Some((head, inp)) =>
        if head == self.e { Success(self.e.clone(), inp) }
        else { Failure(format!("Could not find matching char. found: {:?} required: {:?}", head, self.e)) },
      None => Failure(format!("Could not match char {:?} because EOI.", self.e)),
    }
  }
}

pub fn elem<A: Clone + Eq>(e: A) -> Elem<A> {
  Elem{ e: e }
}

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
pub struct Elems<A> {
  priv elems: ~[A]
}

impl<A: Clone + Eq, S: State<A>> Parser<~[A], S> for Elems<A> {
  fn run(&self, input: S) -> ParseResult<~[A], S> {
    let mut state = input.clone();

    for elem in self.elems.iter() {
      match state.head() {
        Some((ref elem_, ref state_)) if elem == elem_ => state = state_.clone(),
        Some((ref elem_, _)) =>
          return Failure(format!("Could not find matching string. found: {:?} required: {:?}", elem_, elem)),
        None =>
          return Failure(format!("Could not match string {:?} because EOI.", self.elems)),
      }
    }

    Success(self.elems.clone(), state)
  }
}

pub fn elems<A: Clone + Eq>(elems: ~[A]) -> Elems<A> {
  Elems{ elems: elems }
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
  use parser::{Parser, Left, Right, TupleParser};
  use state::{StrState};
  use super::{success, peek, chr, text, anyOfChar, Char};

  fn input() -> StrState {
     StrState::new(~"Hello, 世界")
  }

  #[test]
  fn test_success() {
    let (_, rest) = success(()).run(input()).unwrap();

    assert_eq!(rest.content(), ~"Hello, 世界");
  }

  #[test]
  fn test_peek() {
    let (head, rest) = peek().run(input()).unwrap();

    assert_eq!(head, 'H');
    assert_eq!(rest.content(), ~"ello, 世界");

    let failure_eof = peek().run(StrState::new(~""));

    assert_eq!(failure_eof.err(), ~"Could not match char because EOI.");
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

  #[test]
  fn test_and(){
    let (res, rest) = chr('H').and(chr('e')).run(input()).unwrap();

    assert_eq!(res, ('H', 'e'));
    assert_eq!(rest.content(), ~"llo, 世界");

    let failure_nomatch = chr('a').and(chr('b')).run(StrState::new(~"aef"));

    assert_eq!(failure_nomatch.err(), ~"Could not find matching char. found: 'e' required: 'b'");

    let (res, rest) = chr('a').and(success(())).run(StrState::new(~"aef")).unwrap();

    assert_eq!(res, ('a', ()));
    assert_eq!(rest.content(), ~"ef");
  }

  #[test]
  fn test_or(){
    let (res, rest) = chr('H').or(chr('b')).run(input()).unwrap();

    assert_eq!(res, Left('H'));
    assert_eq!(rest.content(), ~"ello, 世界");

    let failure_nomatch = chr('a').or(chr('b')).run(StrState::new(~"def"));

    assert_eq!(failure_nomatch.err(), ~"Neither of the parsers matched: \nCould not find matching char. found: 'd' required: 'a'\nCould not find matching char. found: 'd' required: 'b'");

    let (res, rest) = chr('a').or(success(())).run(StrState::new(~"def")).unwrap();

    assert_eq!(res, Right(()));
    assert_eq!(rest.content(), ~"def");
  }

  #[test]
  fn test_map(){
    let (txt, rest) = text(~"He").run(input()).map(|t| *t + " MAPPED").unwrap();

    assert_eq!(txt, ~"He MAPPED");
    assert_eq!(rest.content(), ~"llo, 世界");

    fn toInt(t: ~str) -> Option<int> { from_str::<int>(t) }
    let p1 = text(~"12").map(toInt);
    let (number, rest) = p1.run(StrState::new(~"12abc")).unwrap();

    assert_eq!(number, Some(12));
    assert_eq!(rest.content(), ~"abc");

    let p2 = text(~"23").map(toInt);
    let failure_nomatch = p2.run(StrState::new(~"12abc"));

    assert_eq!(failure_nomatch.err(), ~"Could not find matching string. found: ~\"12\" required: ~\"23\"");
  }

  #[test]
  fn test_flat_map(){
    let p = text(~"12");
    fn flatMapFn(txt: ~str) -> Char {
      match from_str::<int>(txt) {
            Some(..) => chr('a'),
            None => chr('b')
          }
    }
    let p1 = p.flatMap(flatMapFn);
    let (character, rest) = p1.run(StrState::new(~"12abc")).unwrap();

    assert_eq!(character, 'a');
    assert_eq!(rest.content(), ~"bc");

    let failure_nomatch = p1.run(StrState::new(~"23abc"));

    assert_eq!(failure_nomatch.err(), ~"Could not find matching string. found: ~\"23\" required: ~\"12\"");

    fn flatMapFn2(txt: ~str) -> Char { match from_str::<int>(txt) { Some(..) => chr('a'), None => chr('b') } }
    let p2 = text(~"cd").flatMap(flatMapFn2);
    let (character, rest) = p2.run(StrState::new(~"cdba")).unwrap();

    assert_eq!(character, 'b');
    assert_eq!(rest.content(), ~"a");
  }

  #[test]
  fn test_zero_or_one(){
    let p1 = chr('a').zeroOrOne();
    let (character, rest) = p1.run(StrState::new(~"abc")).unwrap();

    assert_eq!(character, Some('a'));
    assert_eq!(rest.content(), ~"bc");
    let (character, rest) = p1.run(rest).unwrap();

    assert_eq!(character, None);
    assert_eq!(rest.content(), ~"bc");
  }

  #[test]
  fn test_zero_or_more(){
    let p1 = chr('a').zeroOrMore();
    let (character, rest) = p1.run(StrState::new(~"aaabc")).unwrap();

    assert_eq!(character, ~['a', 'a', 'a']);
    assert_eq!(rest.content(), ~"bc");
    let (character, rest) = p1.run(rest).unwrap();

    assert_eq!(character, ~[]);
    assert_eq!(rest.content(), ~"bc");
  }

  #[test]
  fn test_one_or_more(){
    let p1 = chr('a').oneOrMore();
    let (character, rest) = p1.run(StrState::new(~"aabc")).unwrap();

    assert_eq!(character, ~['a', 'a']);
    assert_eq!(rest.content(), ~"bc");
    let failure_nomatch = p1.run(rest);

    assert_eq!(failure_nomatch.err(), ~"Could not find matching char. found: 'b' required: 'a'");
  }

  #[test]
  fn test_drop_left() {
    let p1 = chr('a').and(chr('b')).drop_left();
    let (character, rest) = p1.run(StrState::new(~"abc")).unwrap();

    assert_eq!(character, 'b');
    assert_eq!(rest.content(), ~"c");

    let failure_nomatch = p1.run(rest);

    assert_eq!(failure_nomatch.err(), ~"Could not find matching char. found: 'c' required: 'a'");
  }
}
