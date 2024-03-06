use std::{fs, io::Read, path::Path};

use anyhow::Context as _;
use roxmltree::{Document, Node};
use simplecss::StyleSheet;
use url::Url;

use crate::{
    len::Len,
    style::{self, Style, Styling},
    util::{normalize_url, parse_hyperlink, trim_end_in_place},
    Author,
};

pub struct Epub {
    container: Container,
    metadata: Metadata,
    spine: Spine,
    toc: Toc,
}

impl Epub {
    pub fn from_path(path: impl AsRef<std::path::Path>) -> anyhow::Result<Self> {
        use fs::File;

        let fd = File::open(path)?;
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
        let rootfile = archive.rootfile(&mut buf)?;
        let (manifest, toc_idx) = rootfile.manifest()?;
        let root = rootfile.root_directory().clone();
        let metadata = rootfile.metadata()?;
        let (spine, ncx_idx) = rootfile.spine(&manifest)?;
        let version = match rootfile.version()? {
            2 => Version::V2(ncx_idx.context("missing ncx idx")?),
            3 => Version::V3(toc_idx.context("missing toc idx")?),
            v => anyhow::bail!("unsupported epub version ({v})"),
        };

        let mut container = Container {
            archive,
            manifest,
            root,
        };

        let toc = Rootfile::toc(&mut container, &spine, version)?;

        Ok(Self {
            container,
            metadata,
            spine,
            toc,
        })
    }

    pub fn name(&self) -> &str {
        &self.metadata.title
    }

    pub fn author(&self) -> Option<&Author> {
        self.metadata.creators.first()
    }

    pub fn chapters(
        &self,
    ) -> impl Iterator<Item = &TocEntry> + DoubleEndedIterator + ExactSizeIterator {
        self.toc.0.iter()
    }

    pub fn chapter_count(&self) -> usize {
        self.toc.0.len()
    }
}

#[derive(Debug)]
struct Spine(Vec<usize>);

impl Spine {
    fn manifest_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.iter().copied()
    }
}

#[derive(Debug)]
struct Toc(Vec<TocEntry>);

impl Toc {
    fn entry(&self, idx: usize) -> Option<&TocEntry> {
        self.0.get(idx)
    }
}

#[derive(Debug)]
pub struct TocEntry {
    name: String,
    fragment: Option<String>,
    idx: usize,
    depth: usize,
}

impl TocEntry {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn depth(&self) -> usize {
        self.depth
    }
}

struct XmlNode<'a, 'input: 'a>(Node<'a, 'input>);

impl simplecss::Element for XmlNode<'_, '_> {
    fn parent_element(&self) -> Option<Self> {
        self.0.parent_element().map(XmlNode)
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        self.0.prev_siblings().find(|n| n.is_element()).map(XmlNode)
    }

    fn has_local_name(&self, local_name: &str) -> bool {
        self.0.tag_name().name() == local_name
    }

    fn attribute_matches(&self, local_name: &str, operator: simplecss::AttributeOperator) -> bool {
        self.0
            .attribute(local_name)
            .map_or(false, |v| operator.matches(v))
    }

    fn pseudo_class_matches(&self, class: simplecss::PseudoClass) -> bool {
        match class {
            simplecss::PseudoClass::FirstChild => self.prev_sibling_element().is_none(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CssAttribute {
    Style(Style),
    Align(Align),
}

impl Epub {
    fn entry_in_manifest(&self, entry: usize) -> Option<usize> {
        self.toc
            .0
            .get(entry)
            .and_then(|e| self.spine.0.get(e.idx))
            .copied()
    }

    pub fn traverse_chapter(
        &mut self,
        entry: usize,
        callback: impl FnMut(Content<'_>, Option<Align>),
    ) -> anyhow::Result<(&str, &str)> {
        self.traverse_chapter_with_replacements(entry, &[], callback)
    }

    pub fn traverse_chapter_with_replacements(
        &mut self,
        entry: usize,
        replacements: &'static [(char, &'static str)],
        callback: impl FnMut(Content<'_>, Option<Align>),
    ) -> anyhow::Result<(&str, &str)> {
        // Retrieve chapter data
        let item_idx = self.entry_in_manifest(entry).context("not found")?;
        let mut data = self.container.retrieve(item_idx)?;
        // println!("{}", data);

        // Parse XML document into tree
        // Note that documents are XHTML, so an XML parser is applicable.
        // However, a DTD accessible by `roxmltree` is not provided, and
        // `roxmltree` does not provide a means to inject DTD entity
        // definitions when parsing an XML document.
        // HACK: If (X)HTML entities are found, `roxmltree` will error due to
        // lacking the entity definition. Entities are rare, but existent, in
        // EPUB documents, so in the event they are encountered they are
        // manually replaced with the corresponding UTF-8 data and the
        // document is re-parsed.
        // Potential alternatives:
        // * Inject custom DTD into data
        // * Inject entity definitions via future `roxmltree` API
        //   (Existing issue: https://github.com/RazrFalcon/roxmltree/issues/105)
        // * Swap XML/XHTML parsing library, however `roxmltree` has favourable
        //   characteristics for EPUB content
        let xml = match Document::parse(&data) {
            Err(roxmltree::Error::UnknownEntityReference(name, _)) => {
                let (needle, replacement) = match name.as_ref() {
                    "nbsp" => ("&nbsp;", " "),
                    _ => anyhow::bail!("entity needs adding ({name})"),
                };

                data = data.replace(needle, replacement);
                Document::parse(&data)
            }
            x => x,
        }?;

        let mut containers = xml.root_element().children().filter(Node::is_element);
        let head = containers.next().context("missing head")?;

        // Load all linked stylesheets and inline style tags in <head>
        // These need to be kept alive, as `simplecss` borrows from its input
        let mut raw_stylesheets = Vec::new();
        for node in head.children().filter(Node::is_element) {
            match node.tag_name().name() {
                "link" if node.attribute("rel") == Some("stylesheet") => {
                    let href = node.attribute("href").unwrap();
                    if !href.ends_with("css") {
                        continue;
                    }
                    let css_item = self.container.resolve_hyperlink(item_idx, href)?;
                    let css = self.container.retrieve(css_item)?;
                    raw_stylesheets.push(css);
                }
                "style" if matches!(node.attribute("type"), Some("text/css") | None) => {
                    raw_stylesheets.push(node.text().context("style tag without text")?.to_owned());
                }
                _ => {}
            }
        }

        // Parse those stylesheets into a single `simplecss::StyleSheet`
        let mut stylesheet = StyleSheet::new();
        for style in raw_stylesheets.iter() {
            stylesheet.parse_more(style);
        }

        // Enumerate only CSS rules that need to be matched, all others in the
        // stylesheet can be ignored
        let mut rules = Vec::new();
        for (i, rule) in stylesheet.rules.iter().enumerate() {
            for dec in &rule.declarations {
                match dec.name {
                    "font-style" if dec.value == "italic" || dec.value.contains("oblique") => {
                        rules.push((i, CssAttribute::Style(Style::ITALIC)))
                    }
                    "font-weight"
                        if matches!(dec.value, "bold" | "bolder")
                            || dec.value.parse::<usize>().is_ok_and(|x| x > 400) =>
                    {
                        rules.push((i, CssAttribute::Style(Style::BOLD)))
                    }
                    "text-align" => {
                        let align = match dec.value {
                            "left" => Align::Left,
                            "center" => Align::Center,
                            "right" => Align::Right,
                            "justify" => Align::Left,
                            "inherit" => continue,
                            a => panic!("invalid text-align? ({a})"),
                        };
                        rules.push((i, CssAttribute::Align(align)))
                    }
                    _ => {}
                }
            }
        }

        let mut parser = ChapterParser {
            replacements,
            stylesheet,
            rules,

            text_buf: String::new(),
            styling: Styling::builder(),

            callback,
        };

        let body = containers.next().context("missing body")?;
        parser.run(body, State::default());

        Ok((self.title(), self.toc.entry(entry).unwrap().name.as_ref()))
    }

    pub fn title(&self) -> &str {
        &self.metadata.title
    }
}

#[derive(Default, Clone)]
struct State {
    style: Style,
    align: Option<Align>,
}

struct ChapterParser<'styles, F> {
    replacements: &'static [(char, &'static str)],
    stylesheet: StyleSheet<'styles>,
    rules: Vec<(usize, CssAttribute)>,

    text_buf: String,
    styling: style::Builder<Len>,

    callback: F,
}

impl<'styles, F> ChapterParser<'styles, F>
where
    F: for<'a> FnMut(Content<'a>, Option<Align>),
{
    fn update_style(&self, node: Node, state: &mut State) {
        // TODO apply style from inline style attribute
        let style = &mut state.style;
        let align = &mut state.align;

        for added_style in self.rules.iter().filter_map(|&(i, style)| {
            self.stylesheet.rules[i]
                .selector
                .matches(&XmlNode(node))
                .then_some(style)
        }) {
            match added_style {
                CssAttribute::Style(s) => *style |= s,
                CssAttribute::Align(a) => *align = Some(a),
            }
        }

        match node.tag_name().name() {
            "i" | "em" => *style |= Style::ITALIC,
            "b" | "strong" => *style |= Style::BOLD,
            "center" => *align = Some(Align::Center),
            _ => {}
        }
    }

    fn add_text_to_buf(&mut self, node: Node, style: Style) {
        let text = &mut self.text_buf;

        let s = node.text().unwrap();

        if s.is_empty() {
            return;
        }

        let start = Len::new(text.len(), text.chars().count());

        if s.chars().next().is_some_and(|c| c.is_ascii_whitespace())
            && text.chars().last().is_some()
            && !text.chars().last().unwrap().is_ascii_whitespace()
        {
            text.push(' ');
        }

        for s in s.split_ascii_whitespace() {
            let mut last_end = 0;
            let matcher = |a| self.replacements.iter().any(|&(b, _)| a == b);
            for (start, part) in s.match_indices(matcher) {
                let part = part.chars().next().unwrap();
                let rep_idx = self
                    .replacements
                    .iter()
                    .position(|&(c, _)| c == part)
                    .unwrap();
                let to = self.replacements[rep_idx].1;
                let chunk = &s[last_end..start];
                text.push_str(chunk);
                text.push_str(to);
                last_end = start + part.len_utf8();
            }
            text.push_str(&s[last_end..s.len()]);
            text.push(' ');
        }

        if text.len() > start.bytes && s.chars().last().is_some_and(|c| !c.is_ascii_whitespace()) {
            text.pop();
        }

        let end = Len::new(
            text.len(),
            start.chars + text[start.bytes..].chars().count(),
        );

        self.styling.add(style, start..end);
    }

    fn accumulate_text(&mut self, node: Node, state: &State) {
        self.text_buf.clear();
        self.styling = Styling::builder();
        self.accumulate_text_(node, state.clone());
        trim_end_in_place(&mut self.text_buf);
    }

    fn accumulate_text_(&mut self, node: Node, mut state: State) {
        self.update_style(node, &mut state);

        if node.is_text() {
            self.add_text_to_buf(node, state.style);
        } else if node.tag_name().name().trim() == "br" {
            self.text_buf.push('\n');
        }

        for child in node.children() {
            self.accumulate_text_(child, state.clone());
        }
    }

    fn emit_text(&mut self, kind: TextContentKind, state: &State) {
        if !self.text_buf.is_empty() {
            let content = Content::Textual {
                text: &self.text_buf,
                styling: self.styling.build(),
                kind,
            };
            (self.callback)(content, state.align);
        }
    }

    fn run(&mut self, node: Node, mut state: State) {
        self.update_style(node, &mut state);

        match node.tag_name().name() {
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => {
                self.accumulate_text(node, &state);
                self.emit_text(TextContentKind::Header, &state);
            }
            "p" => {
                self.accumulate_text(node, &state);
                self.emit_text(TextContentKind::Paragraph, &state);
            }
            "blockquote" => {
                self.accumulate_text(node, &state);
                self.emit_text(TextContentKind::Quote, &state);
            }
            n if n == "image" || (n == "img" && node.has_attribute("src")) => {
                (self.callback)(Content::Image, state.align);
            }
            _ => {
                for child in node.children() {
                    self.run(child, state.clone());
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Align {
    Left,
    Center,
    Right,
}

pub enum Content<'a> {
    Textual {
        kind: TextContentKind,
        text: &'a str,
        styling: Styling<Len>,
    },
    Image,
}

pub enum TextContentKind {
    Header,
    Paragraph,
    Quote,
}

#[derive(Debug, Clone)]
pub struct Item {
    name: String,
    path: Url,
    normalized_path: String,
    mime: String,
}

#[derive(Debug)]
pub struct Manifest(Vec<Item>);

impl Manifest {
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

pub trait ZipRead: std::io::Read + std::io::Seek {}
impl<T> ZipRead for T where T: std::io::Read + std::io::Seek {}

pub struct Container {
    archive: Archive,
    manifest: Manifest,
    root: Url,
}

impl Container {
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
        let url: Url = parse_hyperlink(&item.path, href)?;
        self.manifest.item_idx(&url).context("broken epub href")
    }

    pub fn items(&self) -> impl Iterator<Item = &Item> {
        self.manifest.0.iter()
    }
}

struct Archive(zip::ZipArchive<Box<dyn ZipRead>>);

impl Archive {
    fn new(reader: impl std::io::Read + std::io::Seek + 'static) -> anyhow::Result<Self> {
        let reader: Box<dyn ZipRead> = Box::new(reader);
        Ok(Self(zip::ZipArchive::new(reader)?))
    }

    fn read_into(&mut self, file: &str, buf: &mut String) -> anyhow::Result<()> {
        buf.clear();
        self.0.by_name(file)?.read_to_string(buf)?;
        Ok(())
    }

    fn rootfile<'a>(&mut self, buf: &'a mut String) -> anyhow::Result<Rootfile<'a>> {
        self.read_into("META-INF/container.xml", buf)?;
        let container = Document::parse(buf)?;

        let rootfile_path = container
            .descendants()
            .find(|n| n.has_tag_name("rootfile"))
            .context("missing rootfile")
            .and_then(|rf| rf.attribute("full-path").context("rootfile missing path"))?
            .to_owned();

        let root = {
            let path = Path::new(&rootfile_path);
            anyhow::ensure!(path.is_relative(), "rootfile path not relative");
            let p = path.parent().unwrap();
            let mut path_str = p.to_string_lossy().into_owned();
            path_str.push('/');
            let url = Url::parse("epub:/")?.join(&path_str)?;
            url
        };

        let rootfile = {
            self.read_into(&rootfile_path, buf)?;
            Document::parse(buf)?
        };

        Ok(Rootfile {
            inner: rootfile,
            root,
        })
    }
}

#[derive(Debug)]
struct Metadata {
    identifier: String,
    title: String,
    language: String,
    creators: Vec<crate::Author>,
}

#[derive(Debug, Copy, Clone)]
pub enum Version {
    V2(usize),
    V3(usize),
}

struct Rootfile<'a> {
    inner: Document<'a>,
    root: Url,
}

impl Rootfile<'_> {
    fn version(&self) -> anyhow::Result<u8> {
        let version = self
            .inner
            .root_element()
            .attribute("version")
            .context("missing version")?;
        Ok(version.as_bytes()[0] - b'0')
    }

    fn metadata(&self) -> anyhow::Result<Metadata> {
        let node = self
            .inner
            .root_element()
            .first_element_child()
            .context("missing metadata")?;

        let mut identifier = None;
        let mut title = None;
        let mut language = None;
        let mut creators = Vec::new();
        for child in node.children().filter(Node::is_element) {
            match child.tag_name().name() {
                "identifier" => identifier = child.text().map(ToOwned::to_owned),
                "title" => title = child.text().map(ToOwned::to_owned),
                "language" => language = child.text().map(ToOwned::to_owned),
                "creator" => {
                    if let Some(raw) = child
                        .attribute(("http://www.idpf.org/2007/opf", "file-as"))
                        .or_else(|| child.text())
                    {
                        for name in if raw.contains('&') {
                            raw.split("&")
                        } else {
                            raw.split(" and ")
                        } {
                            if let Some(author) = crate::Author::parse(name) {
                                creators.push(author);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(Metadata {
            identifier: identifier.context("missing identifier")?,
            title: title.context("missing title")?,
            language: language.context("missing language")?,
            creators,
        })
    }

    fn manifest(&self) -> anyhow::Result<(Manifest, Option<usize>)> {
        let mut children = self
            .inner
            .root_element()
            .children()
            .filter(Node::is_element)
            .skip(1);

        let node = children.next().context("rootfile missing manifest")?;

        let mut items = vec![];
        let mut toc_idx = None;
        for child in node.children().filter(Node::is_element) {
            let name = child
                .attribute("id")
                .map(ToOwned::to_owned)
                .context("manifest item missing id")?;
            let href = child
                .attribute("href")
                .context("manifest item missing href")?;
            let path = self.root.join(href)?;
            let normalized_path = normalize_url(&path);
            let mime = child
                .attribute("media-type")
                .map(ToOwned::to_owned)
                .context("manifest item missing mime")?;

            if matches!(child.attribute("properties"), Some("nav")) {
                toc_idx = Some(items.len());
            }

            items.push(Item {
                name,
                path,
                normalized_path,
                mime,
            });
        }

        Ok((Manifest(items), toc_idx))
    }

    fn root_directory(&self) -> &Url {
        &self.root
    }

    fn spine(&self, manifest: &Manifest) -> anyhow::Result<(Spine, Option<usize>)> {
        let node = self
            .inner
            .root_element()
            .children()
            .filter(Node::is_element)
            .skip(2)
            .next()
            .context("missing spine")?;
        let ncx = node
            .attribute("toc")
            .and_then(|name| manifest.item_idx_by_name(name));
        let order = node
            .children()
            .filter_map(|node| node.attribute("idref"))
            .map(|name| manifest.item_idx_by_name(name))
            .collect::<Option<Vec<usize>>>()
            .context("invalid spine")?;
        let spine = Spine(order);
        Ok((spine, ncx))
    }

    fn toc(container: &mut Container, spine: &Spine, version: Version) -> anyhow::Result<Toc> {
        Ok(match version {
            Version::V2(ncx_idx) => Self::toc_v2(container, &spine, ncx_idx)?,
            Version::V3(toc_idx) => Self::toc_v3(container, &spine, toc_idx)?,
        })
    }

    fn toc_v3(container: &mut Container, spine: &Spine, toc_idx: usize) -> anyhow::Result<Toc> {
        fn is_nav(n: &Node) -> bool {
            n.tag_name().name() == "nav"
                && matches!(
                    n.attribute(("http://www.idpf.org/2007/ops", "type")),
                    Some("toc")
                )
        }

        fn find_nav<'a, 'input>(node: Node<'a, 'input>) -> Option<Node<'a, 'input>> {
            for child in node.children() {
                if is_nav(&child) {
                    return Some(child);
                }
                if let Some(nav) = find_nav(child) {
                    return Some(nav);
                }
            }
            None
        }

        let data = container.retrieve(toc_idx)?;
        let xml = Document::parse(&data)?;
        let mut elements = xml.root_element().children().filter(Node::is_element);
        let _head = elements.next().context("toc missing head")?;
        let body = elements.next().context("toc missing body")?;
        let toc_nav = find_nav(body).context("toc missing nav")?;

        let mut entries = Vec::new();
        let list = toc_nav
            .children()
            .filter(Node::is_element)
            .nth(1)
            .context("toc missing navlist")?;
        let toc_uri = container.item_uri(toc_idx);

        fn visit_entries(
            container: &Container,
            spine: &Spine,
            toc_uri: &Url,
            entries: &mut Vec<TocEntry>,
            list: Node,
            depth: usize,
        ) -> anyhow::Result<()> {
            for item in list.children().filter(Node::is_element) {
                let mut elements = item.children().filter(Node::is_element);
                let element = elements.next().context("invalid toc item")?;
                let href = element.attribute("href").context("toc item missing href")?;
                let fragment = href.rsplit_once('#').map(|(_, frag)| frag.to_owned());
                let manifest_idx = container
                    .items()
                    .enumerate()
                    .position(|(idx, _)| {
                        let item_uri = container.item_uri(idx);
                        let href = toc_uri.join(href);
                        if let Ok(href) = href {
                            item_uri.path() == href.path()
                        } else {
                            false
                        }
                    })
                    .context("toc reference missing in manifest")?;
                let idx = spine
                    .manifest_indices()
                    .position(|i| i == manifest_idx)
                    .context("toc reference missing in spine")?;
                let name = element.text().context("toc item missing name")?.to_owned();

                entries.push(TocEntry {
                    name,
                    fragment,
                    idx,
                    depth,
                });

                if let Some(list) = elements.next().filter(|e| e.has_tag_name("ol")) {
                    visit_entries(container, spine, toc_uri, entries, list, depth + 1)?;
                }
            }

            Ok(())
        }

        visit_entries(container, spine, &toc_uri, &mut entries, list, 0)?;

        Ok(Toc(entries))
    }

    fn toc_v2(container: &mut Container, spine: &Spine, ncx_idx: usize) -> anyhow::Result<Toc> {
        let data = container.retrieve(ncx_idx)?;
        let xml = Document::parse(&data).unwrap();

        let nav_map = xml
            .root_element()
            .children()
            .filter(Node::is_element)
            .find(|n| n.tag_name().name() == "navMap")
            .context("toc missing nav map")?;

        fn visit_navpoint(
            container: &Container,
            spine: &Spine,
            entries: &mut Vec<TocEntry>,
            play_order: &mut Vec<usize>,
            nav_point: Node,
            depth: usize,
        ) -> anyhow::Result<()> {
            if let Some(idx) = nav_point
                .attribute("playOrder")
                .map(str::parse)
                .transpose()?
            {
                play_order.push(idx);
            }

            let mut elements = nav_point.children().filter(Node::is_element);
            let name = elements
                .next()
                .and_then(|e| e.first_element_child())
                .and_then(|e| e.text())
                .map(ToOwned::to_owned)
                .context("nav point is missing valid name")?;
            let content = elements
                .next()
                .and_then(|e| e.attribute("src"))
                .context("nav point is missing src attribute")?;

            let (path, fragment) = match content.rsplit_once('#') {
                Some((path, frag)) => (path, Some(frag).map(ToOwned::to_owned)),
                None => (content, None),
            };

            let path = container.root.join(&path).unwrap();
            let norm = normalize_url(&path);

            let idx = container
                .items()
                .position(|item| item.normalized_path == norm)
                .and_then(|idx| spine.manifest_indices().position(|i| i == idx))
                .unwrap();

            entries.push(TocEntry {
                name,
                fragment,
                idx,
                depth,
            });

            for subpoint in elements {
                visit_navpoint(container, spine, entries, play_order, subpoint, depth + 1)?;
            }

            Ok(())
        }

        let mut entries = Vec::new();
        let mut play_order = Vec::new();
        for nav_point in nav_map
            .children()
            .filter(Node::is_element)
            .skip_while(|n| n.tag_name().name() == "navInfo")
        {
            visit_navpoint(
                container,
                spine,
                &mut entries,
                &mut play_order,
                nav_point,
                0,
            )?;
        }

        Ok(Toc(entries))
    }
}
