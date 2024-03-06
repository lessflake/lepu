use anyhow::Context;
use roxmltree::{Document, Node};
use simplecss::StyleSheet;

use crate::{
    epub::Container,
    len::Len,
    style::{self, Style, Styling},
};

pub enum Content<'a> {
    Textual(TextContent<'a>),
    Image,
}

pub struct TextContent<'a> {
    kind: TextualKind,
    text: &'a str,
    len: Len,
    styling: Styling<Len>,
}

impl<'a> TextContent<'a> {
    pub fn text(&self) -> &str {
        self.text
    }

    pub fn kind(&self) -> TextualKind {
        self.kind
    }

    pub fn style_chunks(&self) -> impl Iterator<Item = (&'a str, Style)> + '_ {
        let mut cur = Len::default();
        self.styling.iter(cur, self.len).map(move |(style, len)| {
            let start = cur;
            cur += len;
            (&self.text[start.bytes..cur.bytes], style)
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextualKind {
    Header,
    Paragraph,
    Quote,
}

#[derive(Debug, Clone, Copy)]
pub enum Align {
    Left,
    Center,
    Right,
}

#[derive(Default, Clone)]
struct State {
    style: Style,
    align: Option<Align>,
}

struct Parser<'styles, F> {
    replacements: &'static [(char, &'static str)],
    stylesheet: StyleSheet<'styles>,
    rules: Vec<(usize, CssAttribute)>,

    text_buf: String,
    text_len: Len,
    styling: style::Builder<Len>,

    callback: F,
}

pub fn traverse(
    container: &mut Container,
    index: usize,
    replacements: &'static [(char, &'static str)],
    callback: impl FnMut(Content<'_>, Option<Align>),
) -> anyhow::Result<()> {
    // Retrieve chapter data
    let mut data = container.retrieve(index)?;

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
                let css_item = container.resolve_hyperlink(index, href)?;
                let css = container.retrieve(css_item)?;
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

    let mut parser = Parser {
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

impl<'styles, F> Parser<'styles, F>
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

        let start = self.text_len;

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

    fn accumulate_text(&mut self, node: Node, state: &State) {
        self.text_buf.clear();
        self.text_len = Len::default();
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

    fn emit_text(&mut self, kind: TextualKind, state: &State) {
        if !self.text_buf.is_empty() {
            let content = TextContent {
                text: &self.text_buf,
                styling: self.styling.build(),
                len: self.text_len,
                kind,
            };
            (self.callback)(Content::Textual(content), state.align);
        }
    }

    fn run(&mut self, node: Node, mut state: State) {
        self.update_style(node, &mut state);

        match node.tag_name().name() {
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => {
                self.accumulate_text(node, &state);
                self.emit_text(TextualKind::Header, &state);
            }
            "p" => {
                self.accumulate_text(node, &state);
                self.emit_text(TextualKind::Paragraph, &state);
            }
            "blockquote" => {
                self.accumulate_text(node, &state);
                self.emit_text(TextualKind::Quote, &state);
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

fn trim_end_in_place(s: &mut String) -> usize {
    let mut count = 0;
    while matches!(s.chars().last(), Some(c) if c.is_whitespace()) {
        count += 1;
        s.pop();
    }
    count
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
