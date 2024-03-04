use std::marker::PhantomData;

bitflags::bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Style: u8 {
        const ITALIC = 0b1;
        const BOLD   = 0b10;
    }
}

#[derive(Debug)]
pub struct Styling<T> {
    starts: Vec<Range<Start, T>>,
    ends: Vec<Range<End, T>>,
}

impl<T> Styling<T> {
    pub fn builder() -> Builder<T> {
        Builder { styles: Vec::new() }
    }
}

impl<T> Styling<T>
where
    T: std::ops::AddAssign + std::ops::SubAssign + Ord + Copy,
{
    pub fn add_from_disjoint_other(&mut self, mut other: Self, offset: T) {
        other.offset(offset);
        let Self { starts, ends } = other;
        self.starts.extend(starts);
        self.ends.extend(ends);
    }

    fn offset(&mut self, offset: T) {
        for thing in self
            .starts
            .iter_mut()
            .flat_map(|s| [&mut s.range.start, &mut s.range.end])
            .chain(
                self.ends
                    .iter_mut()
                    .flat_map(|s| [&mut s.range.start, &mut s.range.end]),
            )
        {
            *thing += offset;
        }
    }

    pub fn offset_after(&mut self, i: T, removed: T, added: T) {
        for [start, end] in self
            .starts
            .iter_mut()
            .map(|s| [&mut s.range.start, &mut s.range.end])
            .chain(
                self.ends
                    .iter_mut()
                    .map(|s| [&mut s.range.start, &mut s.range.end]),
            )
        {
            if *start > i {
                *start -= removed;
                *start += added;
            }
            if *end > i {
                *end -= removed;
                *end += added;
            }
        }
    }
}

pub struct Builder<T> {
    styles: Vec<(Style, std::ops::Range<T>)>,
}

impl<T> Builder<T>
where
    T: Ord + Copy,
{
    pub fn add(&mut self, s: Style, r: std::ops::Range<T>) -> &mut Self {
        'outer: for style in s.iter() {
            for (os, or) in &mut self.styles {
                if style == *os && r.start <= or.end && r.end >= or.start {
                    *or = r.start.min(or.start)..r.end.max(or.end);
                    continue 'outer;
                }
            }

            self.styles.push((style, r.clone()));
        }
        self
    }

    pub fn build(&mut self) -> Styling<T> {
        fn pair<T: Clone>(
            (style, range): &(Style, std::ops::Range<T>),
        ) -> (Range<Start, T>, Range<End, T>) {
            (
                Range::new(*style, range.clone()),
                Range::new(*style, range.clone()),
            )
        }
        let (mut starts, mut ends): (Vec<_>, Vec<_>) = self.styles.iter().map(pair).unzip();
        fn cmp<T: Ord>(a: &impl StyleRange<T>, b: &impl StyleRange<T>) -> std::cmp::Ordering {
            a.idx().cmp(&b.idx()).then(a.style().cmp(&b.style()))
        }
        starts.sort_unstable_by(cmp);
        ends.sort_unstable_by(cmp);
        Styling { starts, ends }
    }
}

impl<T: Ord + Copy> Styling<T> {
    pub fn iter(&self, start: T, end: T) -> StylingIter<'_, T> {
        // pub fn iter(&self, range: impl std::ops::RangeBounds<T>) -> StylingIter<'_, T> {
        //     let start = match range.start_bound() {
        //         std::ops::Bound::Unbounded => self.starts[0].range.start,
        //         std::ops::Bound::Included(&x) => x,
        //     };
        assert!(end >= start);
        let end_idx = self.ends.partition_point(|s| s.range.end <= start);
        let start_idx = self
            .ends
            .get(end_idx)
            .map(|e| (e.range.start, e.style))
            .and_then(|e| {
                self.starts
                    .binary_search_by_key(&e, |s| (s.range.start, s.style))
                    .ok()
            })
            .unwrap_or(end_idx);

        StylingIter {
            starts: self.starts[start_idx..].iter().peekable(),
            ends: self.ends[end_idx..].iter().peekable(),
            style: Style::empty(),
            idx: start,
            end,
            ended: start == end,
        }
    }
}

pub struct StylingIter<'a, T> {
    starts: std::iter::Peekable<std::slice::Iter<'a, Range<Start, T>>>,
    ends: std::iter::Peekable<std::slice::Iter<'a, Range<End, T>>>,
    style: Style,
    idx: T,
    end: T,
    ended: bool,
}

impl<T> Iterator for StylingIter<'_, T>
where
    T: Ord + Copy + std::ops::Sub<T, Output = T>,
{
    type Item = (Style, <T as std::ops::Sub>::Output);

    fn next(&mut self) -> Option<Self::Item> {
        fn apply_prior_styles<T: Ord>(
            mut style: Style,
            it: &mut std::iter::Peekable<std::slice::Iter<'_, impl StyleRange<T>>>,
            idx: T,
        ) -> Style {
            while let Some(peek) = it.peek() {
                if peek.idx() <= idx {
                    style = peek.apply(style);
                } else {
                    break;
                }
                let _ = it.next();
            }
            style
        }

        if self.ended {
            return None;
        }

        self.style = apply_prior_styles(self.style, &mut self.ends, self.idx);
        self.style = apply_prior_styles(self.style, &mut self.starts, self.idx);

        let mut next_pos = match (self.starts.peek(), self.ends.peek()) {
            (Some(s), Some(e)) => s.range.start.min(e.range.end),
            (None, Some(e)) => e.range.end,
            (Some(s), None) => s.range.start,
            _ => {
                self.ended = true;
                self.end
            }
        };

        if next_pos <= self.idx {
            self.ended = true;
            return None;
        } else if next_pos >= self.end {
            self.ended = true;
            next_pos = self.end;
        }

        let len = next_pos - self.idx;
        self.idx = next_pos;
        Some((self.style, len))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Range<Marker, T> {
    style: Style,
    range: std::ops::Range<T>,
    phantom: PhantomData<Marker>,
}

impl<Marker, T> Range<Marker, T> {
    fn new(style: Style, range: std::ops::Range<T>) -> Self {
        Self {
            style,
            range,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Start;

#[derive(Debug, Clone, PartialEq, Eq)]
struct End;

trait StyleRange<T> {
    fn idx(&self) -> T;
    fn style(&self) -> Style;
    fn apply(&self, style: Style) -> Style;
}

impl<T: Copy> StyleRange<T> for Range<Start, T> {
    fn idx(&self) -> T {
        self.range.start
    }

    fn style(&self) -> Style {
        self.style
    }

    fn apply(&self, style: Style) -> Style {
        style | self.style
    }
}

impl<T: Copy> StyleRange<T> for Range<End, T> {
    fn idx(&self) -> T {
        self.range.end
    }

    fn style(&self) -> Style {
        self.style
    }

    fn apply(&self, style: Style) -> Style {
        style & !self.style
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        let styles = Styling::builder()
            .add(Style::BOLD, 3..7)
            .add(Style::ITALIC, 0..2)
            .add(Style::ITALIC, 5..10)
            .build();

        let res: Vec<_> = styles.iter(1, 10).collect();
        assert_eq!(
            &res[..],
            &[
                (Style::ITALIC, 1),
                (Style::empty(), 1),
                (Style::BOLD, 2),
                (Style::ITALIC | Style::BOLD, 2),
                (Style::ITALIC, 3),
            ]
        );
    }

    #[test]
    fn it_decomposes_style_flags() {
        let styles = Styling::builder()
            .add(Style::ITALIC | Style::BOLD, 0..1)
            .build();

        let expected = &[
            Range::new(Style::ITALIC, 0..1),
            Range::new(Style::BOLD, 0..1),
        ];
        assert_eq!(&styles.starts[..], expected);
        let expected = &[
            Range::new(Style::ITALIC, 0..1),
            Range::new(Style::BOLD, 0..1),
        ];
        assert_eq!(&styles.ends[..], expected);
    }

    #[test]
    fn it_merges_overlapping_ranges() {
        let styles = Styling::builder()
            .add(Style::ITALIC, 0..2)
            .add(Style::ITALIC, 2..4)
            .add(Style::ITALIC, 3..5)
            .build();

        let expected = &[Range::new(Style::ITALIC, 0..5)];
        assert_eq!(&styles.starts[..], expected);
        let expected = &[Range::new(Style::ITALIC, 0..5)];
        assert_eq!(&styles.ends[..], expected);
    }

    #[test]
    fn it_handles_the_absence_of_styles() {
        let styles = Styling::<usize>::builder().build();
        assert_eq!(styles.iter(0, 10).next(), Some((Style::empty(), 10)));
    }

    #[test]
    fn it_handles_zero_range() {
        let styles = Styling::<usize>::builder().build();
        assert_eq!(styles.iter(0, 0).next(), None);
    }
}
