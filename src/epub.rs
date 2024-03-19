use std::borrow::Cow;

use anyhow::Context as _;

use crate::{
    content,
    parse::{self, Parser},
    uri::Uri,
    zip::Zip,
    Align, Author, Content,
};

/// Represents an EPUB document.
pub struct Epub {
    container: Container,
    metadata: Metadata,
    spine: Spine,
    toc: Toc,
}

impl Epub {
    /// Parse raw bytes into an [`Epub`].
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
        let container = Container::new(zip, manifest, root);
        let toc = parse::toc(&container, &spine, version)?;

        Ok(Self {
            container,
            metadata,
            spine,
            toc,
        })
    }

    /// Traverses an EPUB content document (i.e. an XML tree), keeping track
    /// of current style applied and calling `callback` for every atom of content,
    /// i.e. every header, paragraph and image.
    pub fn traverse_chapter(
        &self,
        entry: usize,
        callback: impl FnMut(content::Context<'_>, Content<'_>, Option<Align>),
    ) -> anyhow::Result<()> {
        self.traverse_chapter_with_replacements(entry, &[], callback)
    }

    /// Same as [`traverse_chapter`], but takes an array of pairs of `char`s to
    /// scan for and `&str`s to replace them with, i.e. if it is necessary to
    /// convert unicode `…` (ellipsis) characters into the literal `...`.
    pub fn traverse_chapter_with_replacements(
        &self,
        entry: usize,
        replacements: &'static [(char, &'static str)],
        callback: impl FnMut(content::Context<'_>, Content<'_>, Option<Align>),
    ) -> anyhow::Result<()> {
        let manifest_idx = self.spine.get(entry).context("not in spine")?;
        content::traverse(&self.container, manifest_idx, replacements, callback)?;
        Ok(())
    }

    /// The title of this EPUB document.
    pub fn title(&self) -> &str {
        &self.metadata.title
    }

    /// The author of this EPUB document.
    pub fn author(&self) -> Option<&Author> {
        self.metadata.creators.first()
    }

    /// The unique identifier of this EPUB document.
    pub fn identifier(&self) -> &str {
        &self.metadata.identifier
    }

    /// The language of this EPUB document.
    pub fn language(&self) -> &str {
        &self.metadata.language
    }

    /// An iterator over entries in the table of contents.
    pub fn chapters(&self) -> impl Iterator<Item = &Chapter> {
        self.toc.0.iter()
    }

    /// Get a table of contents entry from its index in the table of contents.
    pub fn chapter_by_toc_index(&self, idx: usize) -> Option<&Chapter> {
        self.toc.entry(idx)
    }

    /// The number of entries in the table of contents.
    pub fn chapter_count(&self) -> usize {
        self.toc.0.len()
    }

    /// The number of content documents in this EPUB document.
    pub fn document_count(&self) -> usize {
        self.spine.0.len()
    }
}

/// An EPUB's spine.
/// See `https://www.w3.org/TR/epub-33/#sec-spine-elem`.
#[derive(Debug)]
pub(crate) struct Spine(Vec<usize>);

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

/// An EPUB's table of contents.
/// See `https://www.w3.org/TR/epub-33/#sec-nav-toc`.
/// Note that with EPUB version 2, this derives from the NCX.
/// See `https://www.w3.org/TR/epub-33/#sec-opf2-ncx`.
#[derive(Debug)]
pub(crate) struct Toc(Vec<Chapter>);

impl Toc {
    pub fn new(entries: Vec<Chapter>) -> Self {
        Self(entries)
    }

    /// Retrieve the entry at position `idx`.
    pub fn entry(&self, idx: usize) -> Option<&Chapter> {
        // Requires traversing the tree to find the nth element.
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
                // `num_children` includes children owned by children, recursively.
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

/// An entry in the table of contents ([`Toc`]).
#[derive(Debug)]
pub struct Chapter {
    name: String,
    _fragment: Option<String>,
    spine_index: usize,
    toc_index: usize,
    depth: usize,

    // As entries can have subentries, this is represented as a tree.
    children: Vec<Chapter>,
    num_children: usize,
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

/// An EPUB's major version.
#[derive(Debug, Copy, Clone)]
pub enum Version {
    V2(usize),
    V3(usize),
}

/// Metadata for an item in an EPUB manifest.
#[derive(Debug, Clone)]
pub struct Item {
    name: String,
    path: Uri,
    // To avoid recalculation when a case-insensitive comparison and/or or
    // comparison treating percent-encoded and non-percent-encoded paths
    // as equal needs to be made.
    normalized_path: String,
    mime: String,
}

impl Item {
    pub fn new(name: String, path: Uri, mime: String) -> Self {
        let normalized_path = path.normalize_url();
        Self {
            name,
            path,
            normalized_path,
            mime,
        }
    }

    pub fn normalized_path(&self) -> &str {
        &self.normalized_path
    }

    pub fn mime(&self) -> &str {
        &self.mime
    }
}

/// An EPUB's manifest. This is a list of resources in an EPUB document.
/// See `https://www.w3.org/TR/epub-33/#sec-manifest-elem`.
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

/// An abstraction over an EPUB container. Allows for the extraction of items listed in a `Manifest`
/// from the underlying [OCF ZIP container](https://www.w3.org/TR/epub-33/#dfn-ocf-zip-container).
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

    /// Retrieve a resource from the container, given an index of an item
    /// in its [`Manifest`].
    pub fn retrieve(&self, item: usize) -> anyhow::Result<Cow<[u8]>> {
        let item = &self.manifest.0[item];
        let data = self.zip.read(item.path.path()).unwrap();
        Ok(data)
    }

    /// Retrieves metadata about an item in this container's [`Manifest`].
    pub fn item(&self, item: usize) -> Option<&Item> {
        self.manifest.0.get(item)
    }

    /// Retrieves an item's path in the OCF ZIP container.
    pub fn item_uri(&self, idx: usize) -> &Uri {
        &self.manifest.0[idx].path
    }

    /// Locate a resource by following a relative hyperlink from a given item index.
    pub fn resolve_hyperlink(&self, item: usize, href: &str) -> anyhow::Result<usize> {
        let url = self.item_uri(item).join_from_parent(href)?;
        self.manifest.item_idx(&url).context("broken epub href")
    }

    /// Iterator over [`Item`]s listed in this container's [`Manifest`].
    pub fn items(&self) -> impl Iterator<Item = &Item> {
        self.manifest.0.iter()
    }
}
