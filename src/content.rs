use std::borrow::Cow;

use anyhow::Context as _;
use roxmltree::{Document, Node, ParsingOptions};
use simplecss::StyleSheet;

use crate::{
    epub::Container,
    len::Len,
    style::{self, Style, Styling},
};

// An item of content, such as a paragraph or an image.
pub enum Content<'a> {
    Textual(Text<'a>),
    Image(Item<'a>),
}

/// Represents a unit of textual content, such as a paragraph.
pub struct Text<'a> {
    kind: TextKind,
    text: &'a str,
    len: Len,
    styling: Styling<Len>,
}

impl<'a> Text<'a> {
    /// Get raw text content.
    pub fn text(&self) -> &str {
        self.text
    }

    /// What kind of text element does this represent?
    pub fn kind(&self) -> TextKind {
        self.kind
    }

    /// Iterate through text chunked by its [`Style`].
    pub fn style_chunks(&self) -> impl Iterator<Item = (&'a str, Style)> + '_ {
        let mut cur = Len::default();
        self.styling.iter(cur, self.len).map(move |(style, len)| {
            let start = cur;
            cur += len;
            (&self.text[start.bytes..cur.bytes], style)
        })
    }
}

/// What kind of text element [`Text`] represents.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextKind {
    Header,
    Paragraph,
    Quote,
}

/// Visual alignment of an item of content.
#[derive(Debug, Clone, Copy)]
pub enum Align {
    Left,
    Center,
    Right,
    Justify,
}

#[derive(Default, Clone)]
struct State {
    style: Style,
    align: Option<Align>,
}

struct Parser<'styles, 'a, F> {
    ctx: Context<'a>,
    replacements: &'static [(char, &'static str)],
    stylesheet: StyleSheet<'styles>,
    rules: Vec<(usize, CssAttribute)>,

    text_buf: String,
    text_len: Len,
    styling: style::Builder<Len>,

    callback: F,
}

// EPUB content document traversal implementation.
pub(crate) fn traverse<'a>(
    container: &'a Container,
    index: usize,
    replacements: &'static [(char, &'static str)],
    callback: impl FnMut(Context<'a>, Content<'_>, Option<Align>),
) -> anyhow::Result<()> {
    // Retrieve chapter data
    let data = container.retrieve(index)?;
    let mut s = String::from_utf8(data.into_owned()).unwrap();

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
    let xml = match Document::parse_with_options(
        &s,
        ParsingOptions {
            allow_dtd: true,
            ..Default::default()
        },
    ) {
        Err(roxmltree::Error::UnknownEntityReference(name, _)) => {
            let (needle, replacement) = match name.as_ref() {
                "nbsp" => ("&nbsp;", " "),
                _ => anyhow::bail!("entity needs adding ({name})"),
            };

            s = s.replace(needle, replacement);
            Document::parse_with_options(
                &s,
                ParsingOptions {
                    allow_dtd: true,
                    ..Default::default()
                },
            )
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
                let css_item = container.resolve_hyperlink(index, href)?;
                let data = container.retrieve(css_item)?;
                let s = String::from_utf8(data.into_owned()).unwrap();
                raw_stylesheets.push(s);
            }
            "style" if matches!(node.attribute("type"), Some("text/css") | None) => {
                raw_stylesheets.push(node.text().context("style tag without text")?.to_owned());
            }
            _ => {}
        }
    }

    // Parse those stylesheets into a single `simplecss::StyleSheet`
    let mut stylesheet = StyleSheet::new();
    for style in &raw_stylesheets {
        stylesheet.parse_more(style);
    }

    // Enumerate only CSS rules that need to be matched, all others in the
    // stylesheet can be ignored
    let mut rules = Vec::new();
    for (i, rule) in stylesheet.rules.iter().enumerate() {
        for dec in &rule.declarations {
            match dec.name {
                "font-style" if dec.value == "italic" || dec.value.contains("oblique") => {
                    rules.push((i, CssAttribute::Style(Style::ITALIC)));
                }
                "font-weight"
                    if matches!(dec.value, "bold" | "bolder")
                        || dec.value.parse::<usize>().is_ok_and(|x| x > 400) =>
                {
                    rules.push((i, CssAttribute::Style(Style::BOLD)));
                }
                "text-align" => {
                    let align = match dec.value {
                        "left" => Align::Left,
                        "center" => Align::Center,
                        "right" => Align::Right,
                        "justify" => Align::Justify,
                        "inherit" => continue,
                        a => panic!("invalid text-align? ({a})"),
                    };
                    rules.push((i, CssAttribute::Align(align)));
                }
                _ => {}
            }
        }
    }

    let ctx = Context { index, container };

    let mut parser = Parser {
        ctx,
        replacements,
        stylesheet,
        rules,
        text_buf: String::new(),
        text_len: Len::default(),
        styling: Styling::builder(),
        callback,
    };

    let body = containers.next().context("missing body")?;
    parser.run(body, State::default());

    Ok(())
}

/// Context through which resources can be loaded from an `EPUB` through
/// [`Item`] handles.
#[derive(Clone)]
pub struct Context<'a> {
    index: usize,
    container: &'a Container,
}

/// A handle to an `EPUB` manifest item.
pub struct Item<'a> {
    index: usize,
    mime: &'a str,
}

impl Item<'_> {
    /// The MIME type of the resource this item represents.
    pub fn mime(&self) -> &str {
        self.mime
    }
}

impl<'a> Context<'a> {
    /// Load a given [`Item`] from the EPUB container.
    pub fn load(&self, item: &Item) -> anyhow::Result<Cow<'a, [u8]>> {
        let Item { index, .. } = item;
        self.container.retrieve(*index)
    }

    // Locate a resource by following a relative hyperlink
    fn resolve_hyperlink(&self, href: &str) -> anyhow::Result<Item> {
        let index = self.container.resolve_hyperlink(self.index, href)?;
        let mime = &self.container.item(index).unwrap().mime();
        Ok(Item { index, mime })
    }
}

impl<'styles, 'container, F> Parser<'styles, 'container, F>
where
    F: for<'a> FnMut(Context<'container>, Content<'a>, Option<Align>),
{
    // Apply styles based on `node`'s tag and matching CSS rules
    // TODO: apply style from inline style attribute
    fn update_style(&self, node: Node, state: &mut State) {
        let style = &mut state.style;
        let align = &mut state.align;

        // Style from CSS rules (match node's class, id)
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

        // Style from tag name
        match node.tag_name().name() {
            "i" | "em" => *style |= Style::ITALIC,
            "b" | "strong" => *style |= Style::BOLD,
            "center" => *align = Some(Align::Center),
            _ => {}
        }
    }

    // Collect a chunk of text into the text buffer
    fn add_text_to_buf(&mut self, node: Node, style: Style) {
        let text = &mut self.text_buf;

        let s = node.text().unwrap();

        if s.is_empty() {
            return;
        }

        let start = self.text_len;

        // When browsers render text from the DOM, there are several rules
        // for ignoring or squashing whitespace. These rules need to be
        // approximated in the handling of whitespace here, to not yield
        // improperly formatted text from EPUB XHTML documents.
        // See https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace

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

        self.text_len = Len::new(
            text.len(),
            start.chars + text[start.bytes..].chars().count(),
        );

        self.styling.add(style, start..self.text_len);
    }

    fn trim_end_in_place(&mut self) {
        while matches!(self.text_buf.chars().last(), Some(c) if c.is_whitespace()) {
            let c = self.text_buf.pop().unwrap();
            self.text_len.bytes -= c.len_utf8();
            self.text_len.chars -= 1;
        }
    }

    // Recursively process text from the given node and all
    // its children, keeping track of any applied styles
    fn accumulate_text(&mut self, node: Node, state: &State) {
        self.text_buf.clear();
        self.text_len = Len::default();
        self.styling = Styling::builder();
        self.accumulate_text_(node, state.clone());
        self.trim_end_in_place();
    }

    fn accumulate_text_(&mut self, node: Node, mut state: State) {
        self.update_style(node, &mut state);

        if node.is_text() {
            self.add_text_to_buf(node, state.style);
        } else if node.tag_name().name().trim() == "br" {
            self.text_buf.push('\n');
            self.text_len += Len::new(1, 1);
        }

        for child in node.children() {
            self.accumulate_text_(child, state.clone());
        }
    }

    // Call `self.callback` for a completed textual content item ([`Text`]).
    fn emit_text(&mut self, kind: TextKind, state: &State) {
        let content = Text {
            text: &self.text_buf,
            styling: self.styling.build(),
            len: self.text_len,
            kind,
        };
        (self.callback)(self.ctx.clone(), Content::Textual(content), state.align);
    }

    // Traverse the tree, parsing nodes into [`Content`] and emitting to `self.callback`.
    fn run(&mut self, node: Node, mut state: State) {
        self.update_style(node, &mut state);

        match node.tag_name().name() {
            // Header content.
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => {
                self.accumulate_text(node, &state);
                if !self.text_buf.is_empty() {
                    self.emit_text(TextKind::Header, &state);
                    return;
                }
            }
            // Paragraph content.
            "p" => {
                self.accumulate_text(node, &state);
                if !self.text_buf.is_empty() {
                    self.emit_text(TextKind::Paragraph, &state);
                    return;
                }
            }
            // Quote content.
            "blockquote" => {
                self.accumulate_text(node, &state);
                if !self.text_buf.is_empty() {
                    self.emit_text(TextKind::Quote, &state);
                    return;
                }
            }
            // Image content.
            n if n == "image" || (n == "img" && node.has_attribute("src")) => {
                let attr = match n {
                    "img" => node.attribute("src"),
                    "image" => node.attribute(("http://www.w3.org/1999/xlink", "href")),
                    _ => None,
                };
                if let Some(href) = attr {
                    if let Ok(item) = self.ctx.resolve_hyperlink(href) {
                        (self.callback)(self.ctx.clone(), Content::Image(item), state.align);
                    }
                }
                return;
            }
            // Treat text nodes with no markup otherwise as paragraphs.
            n if node.is_text() => {
                self.accumulate_text(node, &state);
                if !self.text_buf.is_empty() {
                    self.emit_text(TextKind::Paragraph, &state);
                    return;
                }
            }
            _ => {}
        }

        for child in node.children() {
            self.run(child, state.clone());
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CssAttribute {
    Style(Style),
    Align(Align),
}

struct XmlNode<'a, 'input: 'a>(Node<'a, 'input>);

impl simplecss::Element for XmlNode<'_, '_> {
    fn parent_element(&self) -> Option<Self> {
        self.0.parent_element().map(XmlNode)
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        self.0.prev_siblings().find(Node::is_element).map(XmlNode)
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
