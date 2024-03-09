use std::{borrow::Cow, collections::BTreeMap};

const MAGIC: &[u8] = &[0x50, 0x4b, 0x05, 0x06];

trait ByteSliceExt {
    fn read_u16(&self) -> u16;
    fn read_u32(&self) -> u32;
    fn read_str(&self, len: usize) -> Result<&str, std::str::Utf8Error>;
}

impl ByteSliceExt for [u8] {
    fn read_u16(&self) -> u16 {
        u16::from_le_bytes(self[..2].try_into().unwrap())
    }

    fn read_u32(&self) -> u32 {
        u32::from_le_bytes(self[..4].try_into().unwrap())
    }

    fn read_str(&self, len: usize) -> Result<&str, std::str::Utf8Error> {
        std::str::from_utf8(&self[..len])
    }
}

#[derive(Debug)]
pub struct Zip {
    data: Box<[u8]>,
    names: BTreeMap<String, Entry>,
}

#[derive(Debug)]
struct Entry {
    offset: usize,
    size: usize,
    compression: Compression,
}

#[derive(Debug)]
enum Compression {
    Uncompressed,
    Deflate,
}

impl Zip {
    pub fn new(data: Vec<u8>) -> Option<Self> {
        let data: Box<[u8]> = data.into();
        let mut names = BTreeMap::new();

        let offset = data
            .windows(MAGIC.len())
            .rev()
            .enumerate()
            .find_map(|(i, a)| (a == MAGIC).then_some(i))?;

        let end_hdr = &data[data.len() - offset - MAGIC.len()..];
        let count = end_hdr[8..].read_u16();
        let cd_offset: usize = end_hdr[16..].read_u32() as _;
        let mut hdr = &data[cd_offset..];
        for _ in 0..count {
            let compression_method = hdr[10..].read_u16();
            let compressed_size: usize = hdr[20..].read_u32() as _;
            let name_length: usize = hdr[28..].read_u16().into();
            let extra_length: usize = hdr[30..].read_u16().into();
            let comment_length: usize = hdr[32..].read_u16().into();
            let local_offset: usize = hdr[42..].read_u32() as _;
            let file_name = hdr[46..].read_str(name_length).ok()?;
            let file_offset = local_offset + 30 + name_length + extra_length;

            names.insert(
                file_name.to_owned(),
                Entry {
                    offset: file_offset,
                    size: compressed_size,
                    compression: match compression_method {
                        0 => Compression::Uncompressed,
                        8 => Compression::Deflate,
                        _ => return None,
                    },
                },
            );

            hdr = &hdr[46 + name_length + extra_length + comment_length..];
        }

        Some(Self { data, names })
    }

    pub fn read(&self, name: &str) -> Option<Cow<[u8]>> {
        let entry = self.names.get(name)?;
        let payload = &self.data[entry.offset..][..entry.size];
        match entry.compression {
            Compression::Uncompressed => Some(payload.into()),
            Compression::Deflate => deflate(payload).map(Into::into),
        }
    }
}

// #[cfg(not(target_arch = "wasm32"))]
fn deflate(payload: &[u8]) -> Option<Vec<u8>> {
    zune_inflate::DeflateDecoder::new(payload)
        .decode_deflate()
        .ok()
}

// #[cfg(target_arch = "wasm32")]
// #[wasm_bindgen::prelude::wasm_bindgen(inline_js = r#"
//     export function js_deflate(buf) {
//         const ds = await new Response(buf).body.pipeThrough(new DecompressionStream("deflate-raw"));
//         const out = await new Response(ds).arrayBuffer();
//         return out;
//     }
// "#)]
// extern "C" {
//     fn js_deflate(_: &[u8]) -> Vec<u8>;
// }

// #[cfg(target_arch = "wasm32")]
// fn deflate(payload: &[u8]) -> Option<Vec<u8>> {
//     Some(js_deflate(payload))
// }
