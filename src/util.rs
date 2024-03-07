use url::Url;

pub fn normalize_url(url: &Url) -> String {
    percent_encoding::percent_decode_str(url.path())
        .decode_utf8()
        .unwrap() // already parsed by `Url`
        .to_lowercase()
}
