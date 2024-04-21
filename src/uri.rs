use std::path::{Component, Path, PathBuf};

use anyhow::Context;

/// Representation of the path of a resource in a [`Container`].
/// Main use is to compute the result of following relative paths, as
/// found in EPUB content documents to refer to resources.
#[derive(Debug, Clone)]
pub struct Uri(PathBuf);

impl Uri {
    pub fn directory_of(s: &str) -> anyhow::Result<Self> {
        let path = Path::new(s);
        anyhow::ensure!(path.is_relative(), "rootfile path not relative");
        Ok(Uri(path.parent().context("rootfile no parent")?.to_owned()))
    }

    /// Compute the result of following a relative path, starting at the
    /// parent of this path.
    pub fn join_from_parent(&self, other: &str) -> anyhow::Result<Self> {
        let mut out = self.clone();
        out.0.pop();
        self.join_(other, out)
    }

    /// Compute the result of following a relative path.
    pub fn join(&self, other: &str) -> anyhow::Result<Self> {
        let out = self.clone();
        self.join_(other, out)
    }

    fn join_(&self, other: &str, mut out: Self) -> anyhow::Result<Self> {
        let other = Path::new(other);
        for c in other.components() {
            match c {
                Component::ParentDir => {
                    out.0.pop();
                }
                Component::Normal(fragment) => out.0.push(fragment),
                Component::CurDir => {}

                Component::Prefix(_) | Component::RootDir => {
                    anyhow::bail!("joining with non-relative path")
                }
            }
        }
        Ok(out)
    }

    pub fn path(&self) -> &str {
        self.0.to_str().unwrap()
    }

    pub fn normalize_url(&self) -> String {
        percent_encoding::percent_decode_str(self.path())
            .decode_utf8()
            .unwrap()
            .to_lowercase()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_file() {
        let a = Uri::directory_of("foo/bar.baz").unwrap();
        let b = Uri::directory_of("foo/bar").unwrap();
        assert_eq!(a.path(), b.path());
        assert_eq!(b.path(), "foo");
    }

    #[test]
    fn path_following() {
        let uri = Uri::directory_of("foo/bar.baz").unwrap();
        let beginning = uri.join("bar.baz").unwrap();
        assert_eq!(beginning.path(), "foo/bar.baz");
        let elsewhere = uri.join_from_parent("bar/foo").unwrap();
        assert_eq!(elsewhere.path(), "bar/foo");
    }

    #[test]
    fn normalized_comparison() {
        let uri = Uri::directory_of("foo/bar.baz").unwrap();
        let a = uri.join("Hello 안녕").unwrap();
        let b = uri.join("hello%20%EC%95%88%EB%85%95").unwrap();
        assert_ne!(a.path(), b.path());
        assert_eq!(a.normalize_url(), b.normalize_url());
    }
}
