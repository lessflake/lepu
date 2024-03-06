#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Author {
    first: String,
    middles: Option<String>,
    surname: String,
}

impl Author {
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
    // }
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
    while buf.chars().last().unwrap().is_whitespace() {
        buf.pop();
    }
    buf
}
