use parser::{Parser, ParseResult, Success, Failure};
use state::{State};

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
pub struct One<A, S> {
  priv e: A
}

impl<A: Clone + Eq, S: State<A>> Parser<A, S> for One<A, S> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match input.head() {
      Some((head, inp)) =>
        if head == self.e { Success(self.e.clone(), inp) }
        else { Failure(format!("Could not find matching element. Found: {:?} required: {:?}", head, self.e)) },
      None => Failure(format!("Could not match char {:?} because EOI.", self.e)),
    }
  }
}

pub fn one<A: Clone + Eq, S: State<A>>(e: A) -> One<A, S> {
  One{ e: e }
}

#[deriving(Eq, ToStr, Clone)]
pub struct Seq<A, S> {
  priv seq: ~[A]
}

impl<A: Clone + Eq, S: State<A>> Parser<~[A], S> for Seq<A, S> {
  fn run(&self, input: S) -> ParseResult<~[A], S> {
    let mut state = input.clone();

    for elem in self.seq.iter() {
      match state.head() {
        Some((ref elem_, ref state_)) if elem == elem_ => state = state_.clone(),
        Some((ref elem_, _)) =>
          return Failure(format!("Could not find matching element. Found: {:?} required: {:?}", elem_, elem)),
        None =>
          return Failure(format!("Could not match element {:?} because EOI.", self.seq)),
      }
    }

    Success(self.seq.clone(), state)
  }
}

pub fn seq<A: Clone + Eq, S: State<A>>(seq: ~[A]) -> Seq<A, S> {
  Seq{ seq: seq }
}

#[deriving(Eq, ToStr, Clone)]
pub struct AnyOf<A, S> {
  priv valid: ~[A]
}

impl<A: Clone + Eq, S: State<A>> Parser<A, S> for AnyOf<A, S> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match input.head() {
      Some((ref c, ref inp)) if self.valid.contains(c) => Success(c.clone(), inp.clone()),
      Some((ref c, _)) =>
        Failure(format!("Could not find matching element. Found: {:?} required any of: {:?}", *c, self.valid)),
      None =>
        Failure(format!("Could not match any of {:?} because EOI.", self.valid)),
    }
  }
}

pub fn anyOf<A: Clone + Eq, S: State<A>>(valid: ~[A]) -> AnyOf<A, S> {
  AnyOf{ valid: valid }
}

#[cfg(test)]
mod tests {
  use parser::{Parser};
  use parser::generic::{success, peek};
  use state::{StrState};

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
}
