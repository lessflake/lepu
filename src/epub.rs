use std::io::Read as _;

use anyhow::Context as _;
use url::Url;

use crate::{content, parse::Parser, util::normalize_url, Align, Author, Content};

pub struct Epub {
    container: Container,
    metadata: Metadata,
    spine: Spine,
    toc: Toc,
}

impl Epub {
    pub fn from_path(path: impl AsRef<std::path::Path>) -> anyhow::Result<Self> {
        let fd = std::fs::File::open(path)?;
        Self::from_reader(std::io::BufReader::new(fd))
    }

    pub fn from_vec(vec: Vec<u8>) -> anyhow::Result<Self> {
        Self::from_reader(std::io::Cursor::new(vec))
    }

    pub fn from_reader(
        reader: impl std::io::Read + std::io::Seek + 'static,
    ) -> anyhow::Result<Self> {
        let mut archive = Archive::new(reader)?;
        let mut buf = String::new();
        let parser = Parser::from_archive(&mut archive, &mut buf)?;
        let (manifest, toc_idx) = parser.manifest()?;
        let root = parser.root_directory().clone();
        let metadata = parser.metadata()?;
        let (spine, ncx_idx) = parser.spine(&manifest)?;
        let version = match parser.version()? {
            2 => Version::V2(ncx_idx.context("missing ncx idx")?),
            3 => Version::V3(toc_idx.context("missing toc idx")?),
            v => anyhow::bail!("unsupported epub version ({v})"),
        };
        let mut container = Container::new(archive, manifest, root);
        let toc = crate::parse::parse_toc(&mut container, &spine, version)?;

        Ok(Self {
            container,
            metadata,
            spine,
            toc,
        })
    }

    pub fn traverse_chapter(
        &mut self,
        entry: usize,
        callback: impl FnMut(Content<'_>, Option<Align>),
    ) -> anyhow::Result<&str> {
        self.traverse_chapter_with_replacements(entry, &[], callback)
    }

    pub fn traverse_chapter_with_replacements(
        &mut self,
        entry: usize,
        replacements: &'static [(char, &'static str)],
        callback: impl FnMut(Content<'_>, Option<Align>),
    ) -> anyhow::Result<&str> {
        let toc_entry = self.toc.entry(entry).context("not in toc")?;
        let manifest_idx = self
            .spine
            .get(toc_entry.spine_index)
            .context("not in spine")?;
        content::traverse(&mut self.container, manifest_idx, replacements, callback)?;
        Ok(toc_entry.name.as_ref())
    }

    pub fn title(&self) -> &str {
        &self.metadata.title
    }

    pub fn author(&self) -> Option<&Author> {
        self.metadata.creators.first()
    }

    pub fn identifier(&self) -> &str {
        &self.metadata.identifier
    }

    pub fn language(&self) -> &str {
        &self.metadata.language
    }

    pub fn chapters(
        &self,
    ) -> impl Iterator<Item = &Chapter> + DoubleEndedIterator + ExactSizeIterator {
        self.toc.0.iter()
    }

    pub fn chapter_count(&self) -> usize {
        self.toc.0.len()
    }
}

#[derive(Debug)]
pub struct Spine(Vec<usize>);

impl Spine {
    pub fn new(order: Vec<usize>) -> Self {
        Self(order)
    }

    pub fn get(&self, idx: usize) -> Option<usize> {
        self.0.get(idx).copied()
    }

    pub fn manifest_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.iter().copied()
    }
}

#[derive(Debug)]
pub struct Toc(Vec<Chapter>);

impl Toc {
    pub fn new(entries: Vec<Chapter>) -> Self {
        Self(entries)
    }

    pub fn entry(&self, idx: usize) -> Option<&Chapter> {
        fn nth_inorder<'a>(
            entries: &'a [Chapter],
            goal: usize,
            cur: &mut usize,
        ) -> Option<&'a Chapter> {
            for node in entries.iter() {
                if *cur == goal {
                    return Some(node);
                }
                *cur += 1;
                if goal < *cur + node.num_children {
                    return nth_inorder(&node.children, goal, cur);
                }
                *cur += node.num_children;
            }

            None
        }

        nth_inorder(&self.0, idx, &mut 0)
    }
}

#[derive(Debug)]
pub struct Chapter {
    name: String,
    _fragment: Option<String>,
    spine_index: usize,
    toc_index: usize,
    depth: usize,

    num_children: usize,
    children: Vec<Chapter>,
}

impl Chapter {
    pub fn new(
        name: String,
        fragment: Option<String>,
        spine_index: usize,
        toc_index: usize,
        depth: usize,
        num_children: usize,
        children: Vec<Self>,
    ) -> Self {
        Self {
            name,
            _fragment: fragment,
            spine_index,
            toc_index,
            depth,
            num_children,
            children,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn children(&self) -> impl Iterator<Item = &Self> {
        self.children.iter()
    }

    pub fn index_in_toc(&self) -> usize {
        self.toc_index
    }
}

#[derive(Debug)]
pub struct Metadata {
    pub identifier: String,
    pub title: String,
    pub language: String,
    pub creators: Vec<crate::Author>,
}

#[derive(Debug, Copy, Clone)]
pub enum Version {
    V2(usize),
    V3(usize),
}

#[derive(Debug, Clone)]
pub struct Item {
    name: String,
    path: Url,
    normalized_path: String,
    _mime: String,
}

impl Item {
    pub fn new(name: String, path: Url, mime: String) -> Self {
        let normalized_path = normalize_url(&path);
        Self {
            name,
            path,
            normalized_path,
            _mime: mime,
        }
    }

    pub fn normalized_path(&self) -> &str {
        &self.normalized_path
    }
}

#[derive(Debug)]
pub struct Manifest(Vec<Item>);

impl Manifest {
    pub fn new(items: Vec<Item>) -> Self {
        Self(items)
    }

    pub fn item_idx(&self, path: &Url) -> Option<usize> {
        let norm = normalize_url(path);
        self.0
            .iter()
            .position(|item| &item.normalized_path == &norm)
    }

    pub fn item_idx_by_name(&self, name: &str) -> Option<usize> {
        self.0.iter().position(|item| item.name == name)
    }
}

pub struct Container {
    archive: Archive,
    manifest: Manifest,
    root: Url,
}

impl Container {
    pub fn new(archive: Archive, manifest: Manifest, root: Url) -> Self {
        Self {
            archive,
            manifest,
            root,
        }
    }

    pub fn root(&self) -> &Url {
        &self.root
    }

    pub fn name_in_archive<'a>(&self, url: &'a Url) -> &'a str {
        // need to strip out leading `/`
        &url.path()[1..]
    }

    pub fn retrieve(&mut self, item: usize) -> anyhow::Result<String> {
        let item = &self.manifest.0[item];
        let abs_path = self.name_in_archive(&item.path);
        let mut data = String::new();
        self.archive.read_into(&abs_path, &mut data)?;
        Ok(data)
    }

    pub fn item_uri(&self, idx: usize) -> &Url {
        &self.manifest.0[idx].path
    }

    pub fn resolve_hyperlink(&self, item: usize, href: &str) -> anyhow::Result<usize> {
        let item = &self.manifest.0[item];
        let url = item.path.join(href)?;
        self.manifest.item_idx(&url).context("broken epub href")
    }

    pub fn items(&self) -> impl Iterator<Item = &Item> {
        self.manifest.0.iter()
    }
}

pub trait ZipRead: std::io::Read + std::io::Seek {}
impl<T> ZipRead for T where T: std::io::Read + std::io::Seek {}

pub struct Archive(zip::ZipArchive<Box<dyn ZipRead>>);

impl Archive {
    pub fn new(reader: impl std::io::Read + std::io::Seek + 'static) -> anyhow::Result<Self> {
        let reader: Box<dyn ZipRead> = Box::new(reader);
        Ok(Self(zip::ZipArchive::new(reader)?))
    }

    pub fn read_into(&mut self, file: &str, buf: &mut String) -> anyhow::Result<()> {
        buf.clear();
        self.0.by_name(file)?.read_to_string(buf)?;
        Ok(())
    }
}
