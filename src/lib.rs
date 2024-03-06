mod epub;
pub use epub::{Chapter, Epub};

mod len;
pub use len::Len;

mod style;
pub use style::Style;

mod author;
pub use author::Author;

mod content;
pub use content::{Align, Content, TextContent, TextualKind};

mod parse;

mod util;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() -> anyhow::Result<()> {
        let mut epub = Epub::from_path(&std::path::Path::new("./example_books/1.epub"))?;
        for chapter in epub.chapters() {
            println!("{:#?}", chapter);
        }
        epub.traverse_chapter(4, |content, _| match content {
            Content::Textual(text) => {
                for (chunk, _) in text.style_chunks() {
                    println!("{}", chunk);
                }
            }
            _ => {}
        })?;
        Ok(())
    }
}
