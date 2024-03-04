use url::Url;

pub fn trim_end_in_place(s: &mut String) -> usize {
    let mut count = 0;
    while matches!(s.chars().last(), Some(c) if c.is_whitespace()) {
        count += 1;
        s.pop();
    }
    count
}

pub fn parse_hyperlink(base: &Url, href: &str) -> anyhow::Result<Url> {
    Ok(base.join(href)?)
}

pub fn normalize_url(url: &Url) -> String {
    let p = url.path();
    let p = percent_encoding::percent_decode_str(p)
        .decode_utf8()
        .unwrap();
    let p = p.to_lowercase();
    p
}
