use std::path::{Component, Path, PathBuf};

use anyhow::Context;

#[derive(Debug, Clone)]
pub struct Uri(PathBuf);

impl Uri {
    pub fn directory_of(s: &str) -> anyhow::Result<Self> {
        let path = Path::new(s);
        anyhow::ensure!(path.is_relative(), "rootfile path not relative");
        Ok(Uri(path.parent().context("rootfile no parent")?.to_owned()))
    }

    pub fn join_from_parent(&self, other: &str) -> anyhow::Result<Self> {
        let mut out = self.clone();
        out.0.pop();
        self.join_(other, out)
    }

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
