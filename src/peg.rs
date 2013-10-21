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

// #[deriving(Eq)]
// pub enum ParseResult<T> {
// 	Success(T, ~StringStream),
// 	Failure(~str),
// }

// pub type Parser<'self, T> = &'self fn<'r>(StringStream<'r>) -> ParseResult<'r, T>;

mod parser {
	use super::{StringStream};

	pub trait Parser<T> {
		fn run_string<'r>(&self, input: &'r str) -> ParseResult<'r, T> {
			self.run_string_stream(StringStream::new(input))
		}

		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, T>;
	}

	pub type ParseResult<'self, T> = Result<(T, StringStream<'self>), ~str>;

	struct Success;

	impl Parser<()> for Success {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, ()> {
			Ok(((), input))
		}
	}

	pub fn success() -> ~Parser<()> {
		~Success as ~Parser<()>
	}

	struct Peek {
		n: int
	}

	impl Parser<char> for Peek {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, char> {
			match input.head() {
				Some((c, inp)) => Ok((c, inp)),
				None => Err(format!("Could not match char because EOI.")),
			}
		}
	}

	pub fn peek() -> ~Parser<char> {
		~Peek{ n: 1 } as ~Parser<char>
	}

	struct Char {
		c: char
	}

	impl Parser<char> for Char {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, char> {
			match input.head() {
				Some((head, inp)) =>
					if head == self.c { Ok((self.c, inp)) }
					else { Err(format!("Could not find matching char. found: '{}' required: '{}'", head, self.c)) },
				None => Err(format!("Could not match char '{}' because EOI.", self.c)),
			}
		}
	}

	pub fn chr(c: char) -> ~Parser<char> {
		~Char{ c: c } as ~Parser<char>
	}

	struct Text {
		txt: ~str
	}

	impl Parser<~str> for Text {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, ~str> {
			match input.take(self.txt.char_len()) {
				Some((t, inp)) =>
					if t == self.txt { Ok((t, inp)) }
					else { Err(format!("Could not find matching string. found: \"{}\" required: \"{}\"", t, self.txt)) },
				None => Err(format!("Could not match string \"{}\" because EOI.", self.txt)),
			}
		}
	}

	pub fn text(txt: ~str) -> ~Parser<~str> {
		~Text{ txt: txt } as ~Parser<~str>
	}

	struct AnyOf {
		valid: ~[char]
	}

	impl Parser<char> for AnyOf {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, char> {
			match input.head() {
				Some((c, inp)) if self.valid.contains(&c) => Ok((c, inp)),
				Some((c, _)) => Err(format!("Could not find matching char. found: '{}' required any of: {:?}", c, self.valid)),
				None => Err(format!("Could not match any of {:?} because EOI.", self.valid)),
			}
		}
	}

	pub fn anyOf(valid: ~[char]) -> ~Parser<char> {
		~AnyOf{ valid: valid } as ~Parser<char>
	}

	struct Or<T, U> {
		p1: ~Parser<T>,
		p2: ~Parser<U>,
	}

	impl<T, U> Parser<Either<T, U>> for Or<T, U> {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, Either<T, U>> {
			match self.p1.run_string_stream(input) {
				Ok((val, inp)) => return Ok((Left(val), inp)),
				Err(err1) =>
					match self.p2.run_string_stream(input) {
						Ok((val, inp)) => return Ok((Right(val), inp)),
						Err(err2) => return Err(format!("Neither of the parsers matched: \n{}\n{}", err1, err2))
					}
			}
		}
	}

	pub fn or<T, U>(p1: ~Parser<T>, p2: ~Parser<U>) -> ~Parser<Either<T, U>> {
		~Or{ p1: p1, p2: p2 } as ~Parser<Either<T, U>>
	}

	struct And<T, U> {
		p1: ~Parser<T>,
		p2: ~Parser<U>,
	}

	impl<T, U> Parser<(T, U)> for And<T, U> {
		fn run_string_stream<'r>(&self, input: StringStream<'r>) -> ParseResult<'r, (T, U)> {
			match self.p1.run_string_stream(input) {
				Ok((val1, inp1)) =>
					match self.p2.run_string_stream(inp1) {
						Ok((val2, inp2)) => Ok(((val1, val2), inp2)),
						Err(err) => Err(err)
					},
				Err(err1) => Err(err1),
			}
		}
	}

	pub fn and<T, U>(p1: ~Parser<T>, p2: ~Parser<U>) -> ~Parser<(T, U)> {
		~And{ p1: p1, p2: p2 } as ~Parser<(T, U)>
	}
}

#[cfg(test)]
mod test {
	use super::StringStream;
	use parser::{success, peek, chr, text, anyOf, or};


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

		assert_eq!(failure_eof, Err(~"Could not match char because EOI."));
	}

	#[test]
	fn test_chr() {
		let (head, rest) = chr('a').run_string("abc").unwrap();

		assert_eq!(head, 'a');
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = chr('b').run_string("abc");

		assert_eq!(failure_nomatch, Err(~"Could not find matching char. found: 'a' required: 'b'"));

		let failure_eof = chr('a').run_string("");

		assert_eq!(failure_eof, Err(~"Could not match char 'a' because EOI."));
	}

	#[test]
	fn test_str() {
		let (txt, rest) = text(~"ab").run_string("abc").unwrap();

		assert_eq!(txt, ~"ab");
		assert_eq!(rest.to_str(), ~"c");

		let failure_nomatch = text(~"af").run_string("abc");

		assert_eq!(failure_nomatch, Err(~"Could not find matching string. found: \"ab\" required: \"af\""));

		let failure_eof = text(~"ab").run_string("");

		assert_eq!(failure_eof, Err(~"Could not match string \"ab\" because EOI."));
	}

	#[test]
	fn test_any_of(){
		let (c, rest) = anyOf(~['a', 'b']).run_string("abc").unwrap();

		assert_eq!(c, 'a');
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = anyOf(~['a', 'b']).run_string("def");

		assert_eq!(failure_nomatch, Err(~"Could not find matching char. found: 'd' required any of: ~['a', 'b']"));

		let failure_eof = anyOf(~['a', 'b']).run_string("");

		assert_eq!(failure_eof, Err(~"Could not match any of ~['a', 'b'] because EOI."));
	}

	#[test]
	fn test_or(){
		let (res, rest) = or(chr('a'), chr('b')).run_string("abc").unwrap();

		assert_eq!(res, Left('a'));
		assert_eq!(rest.to_str(), ~"bc");

		let failure_nomatch = or(chr('a'), chr('b')).run_string("def");

		assert_eq!(failure_nomatch, Err(~"Neither of the parsers matched: \nCould not find matching char. found: 'd' required: 'a'\nCould not find matching char. found: 'd' required: 'b'"));

		let (res, rest) = or(chr('a'), success()).run_string("def").unwrap();

		assert_eq!(res, Right(()));
		assert_eq!(rest.to_str(), ~"def");
	}
}
