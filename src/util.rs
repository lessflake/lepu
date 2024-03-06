use url::Url;

pub fn normalize_url(url: &Url) -> String {
    let p = url.path();
    let p = percent_encoding::percent_decode_str(p)
        .decode_utf8()
        .unwrap();
    let p = p.to_lowercase();
    p
}
