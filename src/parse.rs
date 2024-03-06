use std::path::Path;

use anyhow::Context;
use roxmltree::{Document, Node};
use url::Url;

use crate::{
    epub::{Archive, Container, Item, Manifest, Metadata, Spine, Toc, TocEntry, Version},
    util::normalize_url,
};

pub struct Parser<'a> {
    inner: Document<'a>,
    root: Url,
}

impl<'a> Parser<'a> {
    pub fn from_archive(archive: &mut Archive, buf: &'a mut String) -> anyhow::Result<Self> {
        archive.read_into("META-INF/container.xml", buf)?;
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
            archive.read_into(&rootfile_path, buf)?;
            Document::parse(buf)?
        };

        Ok(Self {
            inner: rootfile,
            root,
        })
    }

    pub fn version(&self) -> anyhow::Result<u8> {
        let version = self
            .inner
            .root_element()
            .attribute("version")
            .context("missing version")?;
        Ok(version.as_bytes()[0] - b'0')
    }

    pub fn metadata(&self) -> anyhow::Result<Metadata> {
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

    pub fn manifest(&self) -> anyhow::Result<(Manifest, Option<usize>)> {
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
            let mime = child
                .attribute("media-type")
                .map(ToOwned::to_owned)
                .context("manifest item missing mime")?;

            if matches!(child.attribute("properties"), Some("nav")) {
                toc_idx = Some(items.len());
            }

            items.push(Item::new(name, path, mime));
        }

        Ok((Manifest::new(items), toc_idx))
    }

    pub fn root_directory(&self) -> &Url {
        &self.root
    }

    pub fn spine(&self, manifest: &Manifest) -> anyhow::Result<(Spine, Option<usize>)> {
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
        let spine = Spine::new(order);
        Ok((spine, ncx))
    }
}

pub fn parse_toc(
    container: &mut Container,
    spine: &Spine,
    version: Version,
) -> anyhow::Result<Toc> {
    Ok(match version {
        Version::V2(ncx_idx) => toc_v2(container, &spine, ncx_idx)?,
        Version::V3(toc_idx) => toc_v3(container, &spine, toc_idx)?,
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

            entries.push(TocEntry::new(name, fragment, idx, depth));

            if let Some(list) = elements.next().filter(|e| e.has_tag_name("ol")) {
                visit_entries(container, spine, toc_uri, entries, list, depth + 1)?;
            }
        }

        Ok(())
    }

    visit_entries(container, spine, &toc_uri, &mut entries, list, 0)?;

    Ok(Toc::new(entries))
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

        let path = container.root().join(&path).unwrap();
        let norm = normalize_url(&path);

        let idx = container
            .items()
            .position(|item| item.normalized_path() == norm)
            .and_then(|idx| spine.manifest_indices().position(|i| i == idx))
            .unwrap();

        entries.push(TocEntry::new(name, fragment, idx, depth));

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

    Ok(Toc::new(entries))
}
