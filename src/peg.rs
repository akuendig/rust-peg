#[link(name = "peg",
	   vers = "0.1.0")];


use std::str::{CharIterator};

pub struct StringStream<'self> {
    priv stream: CharIterator<'self>,
}

impl<'self> StringStream<'self> {

	fn wrap<'r>(it: CharIterator<'r>) -> StringStream<'r> {
		StringStream { stream: it }
	}

	pub fn new<'r>(stream: &'r str) -> StringStream<'r> {
		StringStream { stream: stream.iter() }
	}

	pub fn head<'r>(&self) -> Option<(char, StringStream<'self>)> {
		let mut cpy = self.stream.clone();

		do cpy.next().map |c| {
			(c, StringStream::wrap(cpy))
		}
	}

	pub fn take(&self, n: uint) -> Option<(~str, StringStream<'self>)> {
		let mut cpy = self.stream.clone();
		let mut string = std::str::with_capacity(n);

		for _ in range(0, n) {
			match cpy.next() {
				Some(c) => string.push_char(c),
				None => return None,
			}
		}

		Some((string, StringStream::wrap(cpy)))
	}

	pub fn skip(&self, n: uint) -> Option<StringStream<'self>> {
		let mut cpy = self.stream.clone();

		for _ in range(0, n) {
			if cpy.next().is_none() {
				return None
			}
		}

		return Some(StringStream::wrap(cpy))
	}
}

impl<'self> ToStr for StringStream<'self> {
	fn to_str(&self) -> ~str {
		let mut cpy = self.stream.clone();
		let (lower, _) = cpy.size_hint();
		let mut string = std::str::with_capacity(lower);

		for c in cpy {
			string.push_char(c);
		}

		string
	}
}

impl<'self> Eq for StringStream<'self> {
	fn eq<'r>(&self, other: &'r StringStream) -> bool {
		self.to_str() == other.to_str()
	}
}

mod parser {
	use super::{StringStream};

	pub trait Parser<T> {
		fn run_string<'r>(&self, input: &'r str) -> ParseResult<'r, T> {
			self.run_string_stream(StringStream::new(input))
		}

		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, T>;
	}

	// pub type ParseResult<'self, T> = Result<(T, StringStream<'self>), ~str>;
	#[deriving(Eq, ToStr)]
	pub enum ParseResult<'self, T> {
	    Success(T, StringStream<'self>),
	    Failure(~str),
	}

	impl<'self, T> ParseResult<'self, T> {
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

		pub fn unwrap(self) -> (T, StringStream<'self>) {
			match self {
				Success(v, rest) => (v, rest),
				Failure(err) => fail!(format!("parser::ParseResult::get Failure({})", err)),
			}
		}

		pub fn map<U>(self, f: &fn(v: &T) -> U) -> ParseResult<'self, U> {
			match self {
				Success(v, rest) => Success(f(&v), rest),
				Failure(err) => Failure(err),
			}
		}
	}

	struct Accept;

	impl Parser<()> for Accept {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, ()> {
			Success((), input)
		}
	}

	pub fn success() -> Accept {
		Accept
	}

	pub struct Peek;

	impl Parser<char> for Peek {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, char> {
			match input.head() {
				Some((c, inp)) => Success(c, inp),
				None => Failure(format!("Could not match char because EOI.")),
			}
		}
	}

	pub fn peek() -> Peek {
		Peek
	}

	struct Char {
		priv c: char
	}

	impl Parser<char> for Char {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, char> {
			match input.head() {
				Some((head, inp)) =>
					if head == self.c { Success(self.c, inp) }
					else { Failure(format!("Could not find matching char. found: '{}' required: '{}'", head, self.c)) },
				None => Failure(format!("Could not match char '{}' because EOI.", self.c)),
			}
		}
	}

	pub fn chr(c: char) -> Char {
		Char{ c: c }
	}

	struct Text {
		priv txt: ~str
	}

	impl Parser<~str> for Text {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, ~str> {
			match input.take(self.txt.char_len()) {
				Some((t, inp)) =>
					if t == self.txt { Success(t, inp) }
					else { Failure(format!("Could not find matching string. found: \"{}\" required: \"{}\"", t, self.txt)) },
				None => Failure(format!("Could not match string \"{}\" because EOI.", self.txt)),
			}
		}
	}

	pub fn text(txt: ~str) -> Text {
		Text{ txt: txt }
	}

	struct AnyOf {
		priv valid: ~[char]
	}

	impl Parser<char> for AnyOf {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, char> {
			match input.head() {
				Some((c, inp)) if self.valid.contains(&c) => Success(c, inp),
				Some((c, _)) => Failure(format!("Could not find matching char. found: '{}' required any of: {:?}", c, self.valid)),
				None => Failure(format!("Could not match any of {:?} because EOI.", self.valid)),
			}
		}
	}

	pub fn anyOf(valid: ~[char]) -> AnyOf {
		AnyOf{ valid: valid }
	}

	struct Or<T, U> {
		priv p1: T,
		priv p2: U,
	}

	impl<A, B, T: Parser<A>, U: Parser<B>> Parser<Either<A, B>> for Or<T, U> {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, Either<A, B>> {
			match self.p1.run_string_stream(input) {
				Success(val, inp) => Success(Left(val), inp),
				Failure(err1) =>
					match self.p2.run_string_stream(input) {
						Success(val, inp) => Success(Right(val), inp),
						Failure(err2) => Failure(format!("Neither of the parsers matched: \n{}\n{}", err1, err2))
					}
			}
		}
	}

	pub fn or<A, B, T: Parser<A>, U: Parser<B>>(p1: T, p2: U) -> Or<T, U> {
		Or{ p1: p1, p2: p2 }
	}

	struct And<T, U> {
		priv p1: T,
		priv p2: U,
	}

	impl<A, B, T: Parser<A>, U: Parser<B>> Parser<(A, B)> for And<T, U> {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, (A, B)> {
			match self.p1.run_string_stream(input) {
				Success(val1, inp1) =>
					match self.p2.run_string_stream(inp1) {
						Success(val2, inp2) => Success((val1, val2), inp2),
						Failure(err) => Failure(err)
					},
				Failure(err1) => Failure(err1),
			}
		}
	}

	pub fn and<A, B, T: Parser<A>, U: Parser<B>>(p1: T, p2: U) -> And<T, U> {
		And{ p1: p1, p2: p2 }
	}

	struct Map<A, B, T> {
		p: T,
		f: ~fn(v: A) -> B,
	}

	impl<A, B, T: Parser<A>> Parser<B> for Map<A, B, T> {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, B> {
			match self.p.run_string_stream(input) {
				Success(val, inp) => Success((self.f)(val), inp),
				Failure(err) => Failure(err),
			}
		}
	}

	pub fn map<A, B, T: Parser<A>>(p: T, f: ~fn(v: A) -> B) -> Map<A, B, T> {
		Map{ p: p, f: f }
	}

	struct FlatMap<A, B, T, U> {
		p: T,
		f: ~fn(v: A) -> U,
	}

	impl<A, B, T: Parser<A>, U: Parser<B>> Parser<B> for FlatMap<A, B, T, U> {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, B> {
			match self.p.run_string_stream(input) {
				Success(val, inp) => (self.f)(val).run_string_stream(inp),
				Failure(err) => Failure(err),
			}
		}
	}

	pub fn flatMap<A, B, T: Parser<A>, U: Parser<B>>(p: T, f: ~fn(v: A) -> U) -> FlatMap<A, B, T, U> {
		FlatMap{ p: p, f: f }
	}
}

#[cfg(test)]
mod test {
	use super::StringStream;
	use parser::{success, peek, chr, text, anyOf, or, map, flatMap, Parser};


	fn input() -> StringStream {
		 StringStream::new("Hello, 世界")
	}

	#[test]
	fn test_string_stream_to_str() {
		assert_eq!(input().to_str(), ~"Hello, 世界");
	}

	#[test]
	fn test_string_stream_reevaluate() {
		let input = input();

		let (head1, _) = input.head().unwrap();
		let (head2, _) = input.head().unwrap();

		assert_eq!(head1, 'H');
		assert_eq!(head2, 'H');
	}

	#[test]
	fn test_string_stream_head() {
		let (head, rest) = input().head().unwrap();

		assert_eq!(head, 'H');
		assert_eq!(rest.to_str(), ~"ello, 世界");
	}

	#[test]
	fn test_string_stream_take() {
		let (taken, rest) = input().take(8).unwrap();

		assert_eq!(taken, ~"Hello, 世");
		assert_eq!(rest.to_str(), ~"界");
	}

	#[test]
	fn test_success() {
		let (_, rest) = success().run_string("abc").unwrap();

		assert_eq!(rest.to_str(), ~"abc");
	}

	#[test]
	fn test_peek() {
		let (head, rest) = peek().run_string("abc").unwrap();

		assert_eq!(head, 'a');
		assert_eq!(rest.to_str(), ~"bc");

		let failure_eof = peek().run_string("");

		assert_eq!(failure_eof.get_err(), ~"Could not match char because EOI.");
	}

	#[test]
	fn test_chr() {
		let (head, rest) = chr('a').run_string("abc").unwrap();

		assert_eq!(head, 'a');
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = chr('b').run_string("abc");

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching char. found: 'a' required: 'b'");

		let failure_eof = chr('a').run_string("");

		assert_eq!(failure_eof.get_err(), ~"Could not match char 'a' because EOI.");
	}

	#[test]
	fn test_str() {
		let (txt, rest) = text(~"ab").run_string("abc").unwrap();

		assert_eq!(txt, ~"ab");
		assert_eq!(rest.to_str(), ~"c");

		let failure_nomatch = text(~"af").run_string("abc");

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching string. found: \"ab\" required: \"af\"");

		let failure_eof = text(~"ab").run_string("");

		assert_eq!(failure_eof.get_err(), ~"Could not match string \"ab\" because EOI.");
	}

	#[test]
	fn test_any_of(){
		let (c, rest) = anyOf(~['a', 'b']).run_string("abc").unwrap();

		assert_eq!(c, 'a');
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = anyOf(~['a', 'b']).run_string("def");

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching char. found: 'd' required any of: ~['a', 'b']");

		let failure_eof = anyOf(~['a', 'b']).run_string("");

		assert_eq!(failure_eof.get_err(), ~"Could not match any of ~['a', 'b'] because EOI.");
	}

	#[test]
	fn test_or(){
		let (res, rest) = or(chr('a'), chr('b')).run_string("abc").unwrap();

		assert_eq!(res, Left('a'));
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = or(chr('a'), chr('b')).run_string("def");

		assert_eq!(failure_nomatch.get_err(), ~"Neither of the parsers matched: \nCould not find matching char. found: 'd' required: 'a'\nCould not find matching char. found: 'd' required: 'b'");

		let (res, rest) = or(chr('a'), success()).run_string("def").unwrap();

		assert_eq!(res, Right(()));
		assert_eq!(rest.to_str(), ~"def");
	}

	#[test]
	fn test_map(){
		let (txt, rest) = text(~"ab").run_string("abc").map(|t| *t + " MAPPED").unwrap();

		assert_eq!(txt, ~"ab MAPPED");
		assert_eq!(rest.to_str(), ~"c");

		let p1 = map(text(~"12"), |txt| from_str::<int>(txt));
		let (number, rest) = p1.run_string("12abc").unwrap();

		assert_eq!(number, Some(12));
		assert_eq!(rest.to_str(), ~"abc");

		let p2 = map(text(~"23"), |txt| from_str::<int>(txt));
		let failure_nomatch = p2.run_string("12abc");

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching string. found: \"12\" required: \"23\"");
	}

	#[test]
	fn test_flat_map(){
		let p1 = flatMap(text(~"12"), |txt| match from_str::<int>(txt) { Some(*) => chr('a'), None => chr('b') });
		let (character, rest) = p1.run_string("12abc").unwrap();

		assert_eq!(character, 'a');
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = p1.run_string("23abc");

		assert_eq!(failure_nomatch.get_err(), ~"Could not find matching string. found: \"23\" required: \"12\"");

		let p2 = flatMap(text(~"cd"), |txt| match from_str::<int>(txt) { Some(*) => chr('a'), None => chr('b') });
		let (character, rest) = p2.run_string("cdba").unwrap();

		assert_eq!(character, 'b');
		assert_eq!(rest.to_str(), ~"a");
	}
}
