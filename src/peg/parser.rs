use state::State;

mod state;

// Why the State needs to be part of the Parsers type:
// - We cannot overload the run function. For example, we can
//    run the Char parser only for an input witch has state
//    of chars. Thus we would need to have a bound on the
//    type parameter T of the state. However, we cannot
//    define such a type parameter in the trait, because this
//    would restrict other Parsers, for example Map, to
//    heavily. Thus we need to have a generic type Parameter
//    for the State the Parser is able to work with.
pub trait Parser<A, S>: Clone {
  fn run(&self, input: S) -> ParseResult<A, S>;

  fn or<B, PB: Parser<B, S>>(&self, other: PB) -> Or<Self, PB> {
    Or{ p1: self.clone(), p2: other }
  }

  fn and<B, PB: Parser<B, S>>(&self, other: PB) -> And<Self, PB> {
    And{ p1: self.clone(), p2: other }
  }

  fn map<B>(&self, f: fn (v: A) -> B) -> Map<Self, A, B> {
    Map{ p: self.clone(), f: f }
  }

  fn flatMap<B, PB: Parser<B, S>>(&self, f: fn (v: A) -> PB)
    -> FlatMap<Self, A, PB> {
    FlatMap{ p: self.clone(), f: f }
  }

  fn zeroOrOne(&self) -> ZeroOrOne<Self> {
    ZeroOrOne{ p: self.clone() }
  }

  fn zeroOrMore(&self) -> ZeroOrMore<Self> {
    ZeroOrMore{ p: self.clone() }
  }

  fn oneOrMore(&self) -> OneOrMore<Self> {
    OneOrMore{ p: self.clone() }
  }
}

pub trait TupleParser: Clone {
  fn drop_left(&self) -> DropLeft<Self> {
    DropLeft{ p: self.clone() }
  }

  fn drop_right(&self) -> DropRight<Self> {
    DropRight{ p: self.clone() }
  }
}

impl<A, B, T, S: State<T>, PA: Parser<(A, B), S>> TupleParser for PA {}

#[deriving(Eq, TotalEq, ToStr, Clone)]
pub enum ParseResult<A, I> {
    Success(A, I),
    Failure(~str),
}

impl<A: Clone, I: Clone> ParseResult<A, I> {
  pub fn get(&self) -> A {
    match *self {
      Success(ref v, _) => v.clone(),
      Failure(ref err) => fail!(format!("parser::ParseResult::get Failure({})", *err)),
    }
  }

  pub fn err(&self) -> ~str {
    match *self {
      Success(ref v, ref rest) => fail!(format!("parser::ParseResult::get Success({:?}, {:?})", *v, *rest)),
      Failure(ref err) => err.clone(),
    }
  }

  pub fn unwrap(&self) -> (A, I) {
    match *self {
      Success(ref v, ref rest) => (v.clone(), rest.clone()),
      Failure(ref err) => fail!(format!("parser::ParseResult::get Failure({})", *err)),
    }
  }

  pub fn map<B>(&self, f: |v: &A| -> B) -> ParseResult<B, I> {
    match *self {
      Success(ref v, ref rest) => Success(f(v), rest.clone()),
      Failure(ref err) => Failure(err.clone()),
    }
  }
}

/////////////////
// COMBINATORS //
/////////////////

#[deriving(Eq, ToStr, Clone)]
pub struct Or<PA, PB> {
  priv p1: PA,
  priv p2: PB,
}

#[deriving(Eq, ToStr, Clone)]
pub enum Either<L, R> {
  Left(L),
  Right(R),
}

impl<A, B, T, S: State<T> + Clone, PA: Parser<A, S>, PB: Parser<B, S>>
Parser<Either<A, B>, S> for Or<PA, PB> {
  fn run(&self, input: S) -> ParseResult<Either<A, B>, S> {
    match self.p1.run(input.clone()) {
      Success(val, inp) => Success(Left(val), inp),
      Failure(err1) =>
        match self.p2.run(input.clone()) {
          Success(val, inp) => Success(Right(val), inp),
          Failure(err2) => Failure(format!("Neither of the parsers matched: \n{}\n{}", err1, err2))
        }
    }
  }
}

#[deriving(Eq, ToStr, Clone)]
pub struct And<PA, PB> {
  priv p1: PA,
  priv p2: PB,
}

impl<A, B, T, S: State<T>, PA: Parser<A, S>, PB: Parser<B, S>>
Parser<(A, B), S> for And<PA, PB> {
  fn run(&self, input: S) -> ParseResult<(A, B), S> {
    match self.p1.run(input) {
      Success(val1, inp1) =>
        match self.p2.run(inp1) {
          Success(val2, inp2) => Success((val1, val2), inp2),
          Failure(err) => Failure(err)
        },
      Failure(err1) => Failure(err1),
    }
  }
}

#[deriving(Eq, ToStr, Clone)]
pub struct DropLeft<PA> {
  priv p: PA,
}

impl<A, B, T, S: State<T>, PA: Parser<(A, B), S>> Parser<B, S> for DropLeft<PA> {
  fn run(&self, input: S) -> ParseResult<B, S> {
    match self.p.run(input) {
      Success((_, b), inp) => Success(b, inp),
      Failure(err) => Failure(err),
    }
  }
}

#[deriving(Eq, ToStr, Clone)]
pub struct DropRight<PA> {
  priv p: PA,
}

impl<A, B, T, S: State<T>, PA: Parser<(A, B), S>> Parser<A, S> for DropRight<PA> {
  fn run(&self, input: S) -> ParseResult<A, S> {
    match self.p.run(input) {
      Success((a, _), inp) => Success(a, inp),
      Failure(err) => Failure(err),
    }
  }
}

#[deriving(Clone)]
pub struct Map<PA, A, B> {
  priv p: PA,
  priv f: fn (v: A) -> B,
}

impl<A: Clone, B: Clone, T, S: State<T>, PA: Parser<A, S>> Parser<B, S> for Map<PA, A, B> {
  fn run(&self, input: S) -> ParseResult<B, S> {
    match self.p.run(input) {
      Success(val, inp) => Success((self.f)(val), inp),
      Failure(err) => Failure(err),
    }
  }
}

#[deriving(Clone)]
pub struct FlatMap<PA, A, PB> {
  priv p: PA,
  priv f: fn (v: A) -> PB,
}

impl<A: Clone, B: Clone, T, S: State<T>, PA: Parser<A, S>, PB: Parser<B, S>>
Parser<B, S> for FlatMap<PA, A, PB> {
  fn run(&self, input: S) -> ParseResult<B, S> {
    match self.p.run(input) {
      Success(val, inp) => (self.f)(val).run(inp),
      Failure(err) => Failure(err),
    }
  }
}

#[deriving(Eq, ToStr, Clone)]
pub struct ZeroOrOne<PA> {
  priv p: PA,
}

impl<A, T, S: State<T>, PA: Parser<A, S>> Parser<Option<A>, S> for ZeroOrOne<PA> {
  fn run(&self, input: S) -> ParseResult<Option<A>, S> {
    match self.p.run(input.clone()) {
      Success(val, inp) => Success(Some(val), inp.clone()),
      Failure(..) => Success(None, input),
    }
  }
}

#[deriving(Eq, ToStr, Clone)]
pub struct ZeroOrMore<PA> {
  priv p: PA,
}

impl<A, T, S: State<T>, PA: Parser<A, S>> Parser<~[A], S> for ZeroOrMore<PA> {
  fn run(&self, input: S) -> ParseResult<~[A], S> {
    let mut values = ~[];
    let mut input = input;

    loop {
      match self.p.run(input.clone()) {
        Success(val, inp) => {
          values.push(val);
          input = inp;
        },
        Failure(..) => break,
      }
    }

    Success(values, input)
  }
}

#[deriving(Eq, ToStr, Clone)]
pub struct OneOrMore<PA> {
  priv p: PA,
}

impl<A, T, S: State<T> + Clone, PA: Parser<A, S>> Parser<~[A], S> for OneOrMore<PA> {
  fn run(&self, input: S) -> ParseResult<~[A], S> {
    let mut values = ~[];
    let mut inp = input;

    match self.p.run(inp.clone()) {
      Success(val, inp_) => {
        values.push(val);
        inp = inp_;
      },
      Failure(err) => return Failure(err),
    }

    loop {
      match self.p.run(inp.clone()) {
        Success(val, inp_) => {
          values.push(val);
          inp = inp_;
        },
        Failure(..) => break,
      }
    }

    Success(values, inp)
  }
}

#[cfg(test)]
mod tests {
  use super::{ParseResult, Success, Failure};

  #[test]
  fn parse_result_get() {
    let suc = Success(1, 2);

    assert_eq!(suc.get(), 1);
    assert_eq!(suc.map(|_| ~"abc").get(), ~"abc");

    let f: ParseResult<int, int> = Failure(~"DUMMY");

    assert_eq!(f.err(), ~"DUMMY");
  }
}

