use state::{State, IterState, StrState};

mod state;

pub trait Parser<A, I, O> {
	fn run(&self, input: I) -> ParseResult<A, O>;

	fn or<B, U: Parser<B, I, O>>(self, other: U) -> Or<Self, U> {
		Or{ p1: self, p2: other }
	}

	fn and<B, U: Parser<B, I, O>>(self, p2: U) -> And<Self, U> {
		And{ p1: self, p2: p2 }
	}

	fn map<'r, B>(self, f: 'r |v: A| -> B) -> Map<'r, A, B, Self> {
		Map{ p: self, f: f }
	}

	fn flatMap<'r, B, BO, U: Parser<B, O, BO>>(self, f: 'r |v: A| -> U)
    -> FlatMap<'r, A, Self, U> {
		FlatMap{ p: self, f: f }
	}

	fn zeroOrOne(self) -> ZeroOrOne<Self> {
		ZeroOrOne{ p: self }
	}

	fn zeroOrMore(self) -> ZeroOrMore<Self> {
		ZeroOrMore{ p: self }
	}

	fn oneOrMore(self) -> OneOrMore<Self> {
		OneOrMore{ p: self }
	}
}

// pub type ParseResult<'self, T> = Result<(T, State<'self, char>), ~str>;
#[deriving(Eq, ToStr)]
pub enum ParseResult<T, O> {
    Success(T, O),
    Failure(~str),
}

impl<T, O> ParseResult<T, O> {
	pub fn get(self) -> T {
		match self {
			Success(v, _) => v,
			Failure(err) => fail!(format!("parser::ParseResult::get Failure({})", err)),
		}
	}

	pub fn get_err(self) -> ~str {
		match self {
			Success(v, rest) => fail!(format!("parser::ParseResult::get Success({:?}, {:?})", v, rest)),
			Failure(err) => err.clone(),
		}
	}

	pub fn unwrap(self) -> (T, O) {
		match self {
			Success(v, rest) => (v, rest),
			Failure(err) => fail!(format!("parser::ParseResult::get Failure({})", err)),
		}
	}

	pub fn map<U>(self, f: |v: &T| -> U) -> ParseResult<U, O> {
		match self {
			Success(v, rest) => Success(f(&v), rest),
			Failure(err) => Failure(err),
		}
	}
}

struct Accept;

impl<I> Parser<(), I, I> for Accept {
	fn run<'r>(&self, input: I) -> ParseResult<(), I> {
		Success((), input)
	}
}

pub fn success() -> Accept {
	Accept
}

pub struct Peek;

impl<'self, A: Clone, I: State<A>> Parser<A, I, I> for Peek {
	fn run<'r>(&self, input: I) -> ParseResult<A, I> {
		match input.head() {
			Some((c, inp)) => Success(c, inp),
			None => Failure(format!("Could not match char because EOI.")),
		}
	}
}

pub fn peek() -> Peek {
	Peek
}

struct Elem<A> {
	priv e: A
}

impl<A: Clone + Eq, I: Clone + Iterator<A>> Parser<A, IterState<I>, IterState<I>> for Elem<A> {
	fn run(&self, input: IterState<I>) -> ParseResult<A, IterState<I>> {
		match input.head() {
			Some((head, inp)) =>
				if head == self.e { Success(self.e.clone(), inp) }
				else { Failure(format!("Could not find matching char. found: {:?} required: {:?}", head, self.e)) },
			None => Failure(format!("Could not match char {:?} because EOI.", self.e)),
		}
	}
}

pub fn elem<A: Clone>(e: A) -> Elem<A> {
	Elem{ e: e }
}

struct Char {
    priv c: char
}

impl<'self> Parser<char, StrState<'self>, StrState<'self>> for Char {
	fn run(&self, input: StrState<'self>) -> ParseResult<char, StrState<'self>> {
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

struct Elems<A> {
	priv elems: ~[A]
}

impl<A: Clone + Eq, I: Clone + Iterator<A>> Parser<~[A], IterState<I>, IterState<I>> for Elems<A> {
	fn run(&self, input: IterState<I>) -> ParseResult<~[A], IterState<I>> {
		match input.take(self.elems.len()) {
			Some((t, inp)) =>
				if t == self.elems { Success(t, inp) }
				else { Failure(format!("Could not find matching string. found: {:?} required: {:?}", t, self.elems)) },
			None => Failure(format!("Could not match string {:?} because EOI.", self.elems)),
		}
	}
}

pub fn elems<A: Clone>(elems: ~[A]) -> Elems<A> {
	Elems{ elems: elems }
}

struct Text {
	priv txt: ~str
}

impl<'self> Parser<~str, StrState<'self>, StrState<'self>> for Text {
	fn run(&self, input: StrState<'self>) -> ParseResult<~str, StrState<'self>> {
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

struct AnyOf<A> {
	priv valid: ~[A]
}

impl<A: Clone + Eq, I: State<A> + Clone> Parser<A, I, I> for AnyOf<A> {
	fn run(&self, input: I) -> ParseResult<A, I> {
		match input.head() {
			Some((ref c, ref inp)) if self.valid.contains(c) => Success(c.clone(), inp.clone()),
			Some((ref c, _)) => Failure(format!("Could not find matching char. found: {:?} required any of: {:?}", *c, self.valid)),
			None => Failure(format!("Could not match any of {:?} because EOI.", self.valid)),
		}
	}
}

pub fn anyOf<A: Clone>(valid: ~[A]) -> AnyOf<A> {
	AnyOf{ valid: valid }
}

/////////////////
// COMBINATORS //
/////////////////

struct Or<T, U> {
	priv p1: T,
	priv p2: U,
}

impl<A, B, I: Clone, O, T: Parser<A, I, O>, U: Parser<B, I, O>> Parser<Either<A, B>, I, O> for Or<T, U> {
	fn run(&self, input: I) -> ParseResult<Either<A, B>, O> {
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

struct And<T, U> {
	priv p1: T,
	priv p2: U,
}

impl<A, B, I, O, BO, T: Parser<A, I, O>, U: Parser<B, O, BO>> Parser<(A, B), I, BO> for And<T, U> {
	fn run(&self, input: I) -> ParseResult<(A, B), BO> {
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

struct Map<'self, A, B, T> {
	priv p: T,
	priv f: 'self |v: A| -> B,
}

impl<'self, A, B, I, O, T: Parser<A, I, O>>
Parser<B, I, O> for Map<'self, A, B, T> {
	fn run(&self, input: I) -> ParseResult<B, O> {
		match self.p.run(input) {
			Success(val, inp) => Success((self.f)(val), inp),
			Failure(err) => Failure(err),
		}
	}
}

struct FlatMap<'self, A, T, U> {
	priv p: T,
	priv f: 'self |v: A| -> U,
}

impl<'self, A, B, I: Clone, O: Clone, BO, T: Parser<A, I, O>, U: Parser<B, O, BO>>
Parser<B, I, BO> for FlatMap<'self, A, T, U> {
	fn run(&self, input: I) -> ParseResult<B, BO> {
		match self.p.run(input) {
			Success(val, inp) => (self.f)(val).run(inp),
			Failure(err) => Failure(err),
		}
	}
}

struct ZeroOrOne<T> {
	priv p: T,
}

impl<A, I: Clone, T: Parser<A, I, I>> Parser<Option<A>, I, I> for ZeroOrOne<T> {
	fn run(&self, input: I) -> ParseResult<Option<A>, I> {
		match self.p.run(input.clone()) {
			Success(val, inp) => Success(Some(val), inp.clone()),
			Failure(..) => Success(None, input),
		}
	}
}

struct ZeroOrMore<T> {
	priv p: T,
}

impl<A, I: Clone, T: Parser<A, I, I>> Parser<~[A], I, I> for ZeroOrMore<T> {
	fn run(&self, input: I) -> ParseResult<~[A], I> {
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

struct OneOrMore<T> {
	priv p: T,
}

impl<A, I: Clone, T: Parser<A, I, I>> Parser<~[A], I, I> for OneOrMore<T> {
	fn run(&self, input: I) -> ParseResult<~[A], I> {
		let mut values = ~[];
		let mut input = input;

		match self.p.run(input.clone()) {
			Success(val, inp) => {
				values.push(val);
				input = inp;
			},
			Failure(err) => return Failure(err),
		}

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


#[cfg(test)]
mod test_parser {
	use state::StrState;
	use super::{success, peek, chr, text, anyOf, Parser};

	fn input() -> StrState {
		 StrState::new("Hello, 世界")
	}

	#[test]
	fn test_success() {
		let (_, rest) = success().run(input()).unwrap();

		assert_eq!(rest.content(), &"Hello, 世界");
	}

	#[test]
	fn test_peek() {
		let (head, rest) = peek().run(input()).unwrap();

		assert_eq!(head, 'H');
		assert_eq!(rest.content(), &"ello, 世界");

		let failure_eof = peek().run(StrState::new(""));

		assert_eq!(failure_eof.get_err(), ~"Could not match char because EOI.");
	}

	#[test]
	fn test_chr() {
		let (head, rest) = chr('H').run(input()).unwrap();

		assert_eq!(head, 'H');
		assert_eq!(rest.content(), &"ello, 世界");

		let failure_nomatch = chr('b').run(input());

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching char. found: 'H' required: 'b'");

		let failure_eof = chr('a').run(StrState::new(""));

		assert_eq!(failure_eof.get_err(), ~"Could not match char 'a' because EOI.");
	}

	#[test]
	fn test_str() {
		let (txt, rest) = text(~"Hell").run(input()).unwrap();

		assert_eq!(txt, ~"Hell");
		assert_eq!(rest.content(), &"o, 世界");

		let failure_nomatch = text(~"af").run(input());

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching string. found: \"He\" required: ~\"af\"");

		let failure_eof = text(~"ab").run(StrState::new(""));

		assert_eq!(failure_eof.get_err(), ~"Could not match string ~\"ab\" because EOI.");
	}

	#[test]
	fn test_any_of(){
		let (c, rest) = anyOf(~['H', 'b']).run(input()).unwrap();

		assert_eq!(c, 'H');
		assert_eq!(rest.content(), &"ello, 世界");

		let failure_nomatch = anyOf(~['a', 'b']).run(StrState::new("def"));

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching char. found: 'd' required any of: ~['a', 'b']");

		let failure_eof = anyOf(~['a', 'b']).run(StrState::new(""));

		assert_eq!(failure_eof.get_err(), ~"Could not match any of ~['a', 'b'] because EOI.");
	}

	#[test]
	fn test_and(){
		let (res, rest) = chr('H').and(chr('e')).run(input()).unwrap();

		assert_eq!(res, ('H', 'e'));
		assert_eq!(rest.content(), &"llo, 世界");

		let failure_nomatch = chr('a').and(chr('b')).run(StrState::new("aef"));

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching char. found: 'e' required: 'b'");

		let (res, rest) = chr('a').and(success()).run(StrState::new("aef")).unwrap();

		assert_eq!(res, ('a', ()));
		assert_eq!(rest.content(), &"ef");
	}

	#[test]
	fn test_or(){
		let (res, rest) = chr('H').or(chr('b')).run(input()).unwrap();

		assert_eq!(res, Left('H'));
		assert_eq!(rest.content(), &"ello, 世界");

		let failure_nomatch = chr('a').or(chr('b')).run(StrState::new("def"));

		assert_eq!(failure_nomatch.get_err(), ~"Neither of the parsers matched: \nCould not find matching char. found: 'd' required: 'a'\nCould not find matching char. found: 'd' required: 'b'");

		let (res, rest) = chr('a').or(success()).run(StrState::new("def")).unwrap();

		assert_eq!(res, Right(()));
		assert_eq!(rest.content(), &"def");
	}

	#[test]
	fn test_map(){
		let (txt, rest) = text(~"He").run(input()).map(|t| *t + " MAPPED").unwrap();

		assert_eq!(txt, ~"He MAPPED");
		assert_eq!(rest.content(), &"llo, 世界");

		let p1 = text(~"12").map(|txt| from_str::<int>(txt));
		let (number, rest) = p1.run(StrState::new("12abc")).unwrap();

		assert_eq!(number, Some(12));
		assert_eq!(rest.content(), &"abc");

		let p2 = text(~"23").map(|txt| from_str::<int>(txt));
		let failure_nomatch = p2.run(StrState::new("12abc"));

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching string. found: \"12\" required: ~\"23\"");
	}

	#[test]
	fn test_flat_map(){
		let p1 = text(~"12").flatMap(|txt| match from_str::<int>(txt) { Some(..) => chr('a'), None => chr('b') });
		let (character, rest) = p1.run(StrState::new("12abc")).unwrap();

		assert_eq!(character, 'a');
		assert_eq!(rest.content(), &"bc");

		let failure_nomatch = p1.run(StrState::new("23abc"));

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching string. found: \"23\" required: ~\"12\"");

		let p2 = text(~"cd").flatMap(|txt| match from_str::<int>(txt) { Some(..) => chr('a'), None => chr('b') });
		let (character, rest) = p2.run(StrState::new("cdba")).unwrap();

		assert_eq!(character, 'b');
		assert_eq!(rest.content(), &"a");
	}

	#[test]
	fn test_zero_or_one(){
		let p1 = chr('a').zeroOrOne();
		let (character, rest) = p1.run(StrState::new("abc")).unwrap();

		assert_eq!(character, Some('a'));
		assert_eq!(rest.content(), &"bc");
		let (character, rest) = p1.run(rest).unwrap();

		assert_eq!(character, None);
		assert_eq!(rest.content(), &"bc");
	}

	#[test]
	fn test_zero_or_more(){
		let p1 = chr('a').zeroOrMore();
		let (character, rest) = p1.run(StrState::new("aaabc")).unwrap();

		assert_eq!(character, ~['a', 'a', 'a']);
		assert_eq!(rest.content(), &"bc");
		let (character, rest) = p1.run(rest).unwrap();

		assert_eq!(character, ~[]);
		assert_eq!(rest.content(), &"bc");
	}

	#[test]
	fn test_one_or_more(){
		let p1 = chr('a').oneOrMore();
		let (character, rest) = p1.run(StrState::new("aabc")).unwrap();

		assert_eq!(character, ~['a', 'a']);
		assert_eq!(rest.content(), &"bc");
		let failure_nomatch = p1.run(rest);

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching char. found: 'b' required: 'a'");
	}
}
