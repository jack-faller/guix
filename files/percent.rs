use std::io::{stdin, BufReader, Error, ErrorKind, Read, Result, Write};
fn hex(c: u8) -> Option<u8> {
	match c {
		b'0'..=b'9' => Some(c - b'0'),
		b'A'..=b'F' => Some(c - b'A' + 10),
		b'a'..=b'f' => Some(c - b'a' + 10),
		_ => None,
	}
}
fn encode<S>(s: S) -> Result<()>
where
	S: Iterator<Item = Result<u8>>,
{
	for c in s {
		let c = c?;
		match c {
			b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'-' | b'.' | b'_' | b'~' => {
				print!("{}", c as char)
			}
			_ => print!("%{:02X}", c),
		}
	}
	Ok(())
}
fn decode<S>(mut iter: S) -> Result<()>
where
	S: Iterator<Item = Result<u8>>,
{
	while let Some(c) = iter.next() {
		let c = c?;
		if c == b'%' {
			let a = iter.next();
			let b = iter.next();
			if let (Some(a), Some(b)) = (a, b) {
				let a = a?;
				let b = b?;
				if let (Some(a), Some(b)) = (hex(a), hex(b)) {
					std::io::stdout().write(&[a * 16 + b])?;
				} else {
					eprintln!("Invalid hex sequence {}{}.", a, b);
					return Err(Error::from(ErrorKind::InvalidData));
				}
			} else {
				eprintln!("Hex sequence interrupted by EOF.");
				return Err(Error::from(ErrorKind::UnexpectedEof));
			}
		} else {
			std::io::stdout().write(&[c])?;
		}
	}
	Ok(())
}
struct ByteIterator<T>(T);
impl<T> Iterator for ByteIterator<T>
where
	T: Read,
{
	type Item = Result<u8>;
	fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		let mut buf = [0];
		match self.0.read(&mut buf) {
			Ok(1) => Some(Ok(buf[0])),
			Ok(_) => None,
			Err(e) => Some(Err(e)),
		}
	}
}
fn main() -> Result<()> {
	let mut args = std::env::args();
	let _ = args.next();
	let action = args.next();
	let string = args.next();
	match (action.as_deref(), string.as_deref()) {
		(Some("encode"), Some(s)) => {
			encode(s.bytes().map(|x| Ok(x)))?;
			print!("\n");
			Ok(())
		}
		(Some("decode"), Some(s)) => {
			decode(s.bytes().map(|x| Ok(x)))?;
			print!("\n");
			Ok(())
		}
		(Some("encode"), None) => encode(ByteIterator(BufReader::new(stdin()))),
		(Some("decode"), None) => decode(ByteIterator(BufReader::new(stdin()))),
		(_, _) => {
			eprintln!("Usage: percent [encode/decode] <string>.");
			Err(Error::from(ErrorKind::InvalidInput))
		}
	}
}
