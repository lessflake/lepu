mod epub;
pub use epub::{Chapter, Epub};

mod len;
pub use len::Len;

mod style;
pub use style::Style;

mod author;
pub use author::Author;

mod content;
pub use content::{Align, Content, Text, TextKind};

mod parse;
mod uri;
mod zip;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() -> anyhow::Result<()> {
        let data = std::fs::read("example_books/1.epub")?;
        let epub = Epub::new(data)?;
        epub.traverse_chapter(0, |ctx, content, _| match content {
            Content::Textual(text) => {
                for (chunk, _) in text.style_chunks() {
                    println!("{}", chunk);
                }
            }
            Content::Image(item) => {
                let _data = ctx.load(&item).unwrap();
            }
        })?;
        Ok(())
    }
}
