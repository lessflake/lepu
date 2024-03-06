mod epub;
pub use epub::{Align, Content, Epub};

mod len;
pub use len::Len;

mod style;
pub use style::{Style, Styling, StylingIter};

mod author;
pub use author::Author;

mod util;

#[cfg(test)]
mod tests {
    use self::epub::TextContentKind;

    use super::*;

    #[test]
    fn it_works() -> anyhow::Result<()> {
        let mut epub = Epub::from_path(&std::path::Path::new("./example_books/1.epub"))?;
        let (book_title, chapter_name) = epub.traverse_chapter(5, |content, _| match content {
            Content::Textual { text, kind, .. } => {
                if matches!(kind, TextContentKind::Paragraph) {
                    println!("{}", text);
                }
            }
            _ => {}
        })?;
        Ok(())
    }
}
