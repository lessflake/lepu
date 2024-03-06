#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct Len {
    pub bytes: usize,
    pub chars: usize,
}

impl Len {
    pub fn new(bytes: usize, chars: usize) -> Self {
        Self { bytes, chars }
    }
}

impl std::cmp::PartialOrd for Len {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.bytes.cmp(&other.bytes))
    }
}

impl std::cmp::Ord for Len {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.bytes.cmp(&other.bytes)
    }
}

impl std::ops::Add<Self> for Len {
    type Output = Self;

    fn add(self, Self { bytes, chars }: Self) -> Self::Output {
        Self {
            bytes: self.bytes + bytes,
            chars: self.chars + chars,
        }
    }
}

impl std::ops::AddAssign<Self> for Len {
    fn add_assign(&mut self, rhs: Self) {
        self.bytes += rhs.bytes;
        self.chars += rhs.chars;
    }
}

impl std::ops::Sub<Self> for Len {
    type Output = Self;

    fn sub(self, Self { bytes, chars }: Self) -> Self::Output {
        Self {
            bytes: self.bytes - bytes,
            chars: self.chars - chars,
        }
    }
}

impl std::ops::SubAssign<Self> for Len {
    fn sub_assign(&mut self, rhs: Self) {
        self.bytes -= rhs.bytes;
        self.chars -= rhs.chars;
    }
}
