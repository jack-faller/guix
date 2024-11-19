use std::io::{Error, ErrorKind, Write};
fn hex(c: char) -> Option<u8> {
	match c {
		'0'..='9' => Some(c as u8 - b'0'),
		'A'..='F' => Some(c as u8 - b'A' + 10),
		'a'..='f' => Some(c as u8 - b'a' + 10),
		_ => None,
	}
}
fn main() -> Result<(), Error> {
	let mut args = std::env::args();
	let _ = args.next();
	let action = args.next();
	let string = args.next();
	match (action.as_deref(), string.as_deref()) {
		(Some("encode"), Some(s)) => {
			for c in s.bytes() {
				match c {
					b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'-' | b'.' | b'_' | b'~' => {
						print!("{}", c as char)
					}
					_ => print!("%{:02X}", c),
				}
			}
			print!("\n");
			Ok(())
		}
		(Some("decode"), Some(s)) => {
			let mut iter = s.chars();
			while let Some(c) = iter.next() {
				if c == '%' {
					let a = iter.next();
					let b = iter.next();
					if let (Some(a), Some(b)) = (a, b) {
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
					print!("{}", c);
				}
			}
			print!("\n");
			Ok(())
		}
		(_, _) => {
			eprintln!("Usage: percent [encode/decode] <string>.");
			Err(Error::from(ErrorKind::InvalidInput))
		}
	}
}
