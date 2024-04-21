/// Loosely standardised representation of author name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Author {
    first: String,
    middles: Option<String>,
    surname: String,
}

impl Author {
    /// Parse common EPUB author name representations into a standardised format.
    ///
    /// Attempts to account for shortenings with or without periods, differing
    /// capitalisation and surname-first formats.
    ///
    /// Does not attempt to reconcile shortenings with full names, i.e.
    /// `JRR Tolkien` is not the same as `John RR Tolkien`, `John Tolkien`
    /// or `John Ronald Reuel Tolkien`. It is the same as `J. R. R. Tolkien`,
    /// `J.R.R. Tolkien` and `Tolkien, J.R.R.`.
    pub(crate) fn parse(raw: &str) -> Option<Self> {
        let mut raw = raw.trim();
        if raw.is_empty() || raw == "Unknown" {
            return None;
        };
        let block_caps = !raw.chars().any(char::is_lowercase);
        let mut buf = String::new();
        if !block_caps {
            let (mut first, mut second) = (raw.chars(), raw.chars().skip(1));
            while let (Some(a), Some(b)) = (first.next(), second.next()) {
                buf.push(a);
                if a.is_uppercase() && b.is_uppercase() {
                    buf.push(' ');
                }
            }
            buf.push(raw.chars().last().unwrap());
            raw = &buf;
        }
        let name = raw.to_lowercase();
        let name = name.replace(". ", " ");
        let name = name.replace('.', " ");
        let name = name.trim();
        let comma_count = name.matches(',').count();
        let reversed = comma_count % 2 == 1;
        let (given, surname) = match (comma_count, reversed) {
            (0, _) => name.rsplit_once(' ').unwrap_or((name, "")),
            (_, true) => name.split_once(',').map(|(a, b)| (b, a)).unwrap(),
            (_, false) => name.split_once(',').unwrap(),
        };
        let mut given = given.trim();
        let mut surname = surname.trim();
        let middles = if let Some((middles, real_surname)) = surname.rsplit_once(' ') {
            surname = real_surname.trim_start();
            Some(middles)
        } else if let Some((first, middles)) = given.split_once(' ') {
            given = first.trim_end();
            Some(middles)
        } else {
            None
        };
        Some(Self {
            first: capitalise(given.trim()),
            middles: middles.map(str::trim).map(capitalise),
            surname: capitalise(surname.trim_matches(',').trim()),
        })
    }
}

impl std::fmt::Display for Author {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.first)?;
        f.write_str(" ")?;
        if let Some(middles) = &self.middles {
            f.write_str(middles)?;
            f.write_str(" ")?;
        }
        f.write_str(&self.surname)
    }
}

fn capitalise(s: &str) -> String {
    let mut buf = String::new();
    for word in s.split_whitespace() {
        for ch in word.chars().next().unwrap().to_uppercase() {
            buf.push(ch);
        }
        if word.len() == 1 {
            buf.push_str(". ");
        } else {
            for ch in word.chars().skip(1) {
                buf.push(ch);
            }
        }
    }
    while matches!(buf.chars().last(), Some(c) if c.is_whitespace()) {
        buf.pop();
    }
    buf
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn double_middle() {
        let expected = Author {
            first: "Alice".to_string(),
            middles: Some("A. B.".to_string()),
            surname: "Bob".to_string(),
        };
        let examples = [
            "Alice A.B. Bob",
            "Alice AB Bob",
            "ALICE A. B. BOB",
            "alice a b bob",
        ];
        for ex in examples {
            let author = Author::parse(ex).unwrap();
            assert_eq!(author, expected);
        }
    }

    #[test]
    fn no_middle() {
        let expected = Author {
            first: "Alice".to_string(),
            middles: None,
            surname: "Bob".to_string(),
        };
        let examples = ["Alice Bob", "Alice Bob", "ALICE BOB", "alice bob"];
        for ex in examples {
            let author = Author::parse(ex).unwrap();
            assert_eq!(author, expected);
        }
    }

    #[test]
    fn triple_shortening() {
        let expected = Author {
            first: "A.".to_string(),
            middles: Some("A. B.".to_string()),
            surname: "Bob".to_string(),
        };
        let examples = ["AAB Bob", "Bob, AAB", "A.A.B. Bob", "A. A.B. Bob"];
        for ex in examples {
            let author = Author::parse(ex).unwrap();
            assert_eq!(author, expected);
        }
    }
}
