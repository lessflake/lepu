use std::borrow::Cow;

use anyhow::Context as _;

use crate::{
    content,
    parse::{self, Parser},
    uri::Uri,
    zip::Zip,
    Align, Author, Content,
};

pub struct Epub {
    container: Container,
    metadata: Metadata,
    spine: Spine,
    toc: Toc,
}

impl Epub {
    pub fn new(e: Vec<u8>) -> anyhow::Result<Self> {
        let zip = Zip::new(e).unwrap();
        let (path, uri) = parse::root(&zip)?;
        let data = zip.read(&path).unwrap();
        let s = std::str::from_utf8(&data).unwrap();
        let parser = Parser::new(s, uri)?;
        let (manifest, toc_idx) = parser.manifest()?;
        let root = parser.root_directory().clone();
        let metadata = parser.metadata()?;
        let (spine, ncx_idx) = parser.spine(&manifest)?;
        let version = match parser.version()? {
            2 => Version::V2(ncx_idx.context("missing ncx idx")?),
            3 => Version::V3(toc_idx.context("missing toc idx")?),
            v => anyhow::bail!("unsupported epub version ({v})"),
        };
        let mut container = Container::new(zip, manifest, root);
        let toc = parse::toc(&mut container, &spine, version)?;

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
    ) -> anyhow::Result<()> {
        self.traverse_chapter_with_replacements(entry, &[], callback)
    }

    pub fn traverse_chapter_with_replacements(
        &mut self,
        entry: usize,
        replacements: &'static [(char, &'static str)],
        callback: impl FnMut(Content<'_>, Option<Align>),
    ) -> anyhow::Result<()> {
        let manifest_idx = self.spine.get(entry).context("not in spine")?;
        content::traverse(&mut self.container, manifest_idx, replacements, callback)?;
        Ok(())
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

    pub fn chapters(&self) -> impl Iterator<Item = &Chapter> {
        self.toc.0.iter()
    }

    pub fn chapter_by_toc_index(&self, idx: usize) -> Option<&Chapter> {
        self.toc.entry(idx)
    }

    pub fn chapter_count(&self) -> usize {
        self.toc.0.len()
    }

    pub fn document_count(&self) -> usize {
        self.spine.0.len()
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
            for node in entries {
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

    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }

    pub fn children(&self) -> impl Iterator<Item = &Self> {
        self.children.iter()
    }

    pub fn index_in_toc(&self) -> usize {
        self.toc_index
    }

    pub fn index_in_spine(&self) -> usize {
        self.spine_index
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
    path: Uri,
    normalized_path: String,
    _mime: String,
}

impl Item {
    pub fn new(name: String, path: Uri, mime: String) -> Self {
        let normalized_path = path.normalize_url();
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

    pub fn item_idx(&self, path: &Uri) -> Option<usize> {
        let norm = path.normalize_url();
        self.0.iter().position(|item| item.normalized_path == norm)
    }

    pub fn item_idx_by_name(&self, name: &str) -> Option<usize> {
        self.0.iter().position(|item| item.name == name)
    }
}

pub struct Container {
    zip: Zip,
    manifest: Manifest,
    root: Uri,
}

impl Container {
    pub fn new(zip: Zip, manifest: Manifest, root: Uri) -> Self {
        Self {
            zip,
            manifest,
            root,
        }
    }

    pub fn root(&self) -> &Uri {
        &self.root
    }

    pub fn retrieve(&self, item: usize) -> anyhow::Result<Cow<[u8]>> {
        let item = &self.manifest.0[item];
        let data = self.zip.read(item.path.path()).unwrap();
        Ok(data)
    }

    pub fn item_uri(&self, idx: usize) -> &Uri {
        &self.manifest.0[idx].path
    }

    pub fn resolve_hyperlink(&self, item: usize, href: &str) -> anyhow::Result<usize> {
        let item = &self.manifest.0[item];
        let url = item.path.join_from_parent(href)?;
        self.manifest.item_idx(&url).context("broken epub href")
    }

    pub fn items(&self) -> impl Iterator<Item = &Item> {
        self.manifest.0.iter()
    }
}
