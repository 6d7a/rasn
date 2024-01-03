//! # Decoding XER

use std::io::BufReader;

use xml::{common::XmlVersion, reader::XmlEvent, EventReader, ParserConfig};

use crate::{error::*, types::*, Decode};

use super::{BOOLEAN_TYPE_TAG, BIT_STRING_TYPE_TAG};

macro_rules! error {
    ($kind:ident, $($arg:tt)*) => {
        DecodeError::from(XerDecodeErrorKind::$kind {
            details: format!($($arg)*)
        })
    };
}

macro_rules! tag {
    ($event:ident, $this:ident, $tag:expr) => {
        match $this.next() {
            Ok(XmlEvent::$event { name, .. }) => {
                if name.local_name.as_str() == $tag {
                    Ok(())
                } else {
                    Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                        needed: $tag,
                        found: format!("{name:?}"),
                    }))
                }
            }
            Ok(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $tag,
                found: format!("{elem:?}"),
            })),
            Err(e) => Err(error!(XmlParser, "{e:?}")),
        }
    };
}

macro_rules! value {
    ($this:ident, $parser:ident, $expected:expr) => {{
        match $this.next() {
            Ok(XmlEvent::Characters(s)) => $parser(&s),
            Ok(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $expected,
                found: format!("{elem:?}"),
            })),
            Err(e) => Err(error!(XmlParser, "{e:?}")),
        }
    }};
}

macro_rules! value_or_empty {
    ($this:ident, $tag:expr, $parser:ident, $expected:expr) => {{
        let value = match $this.peek() {
            Some(XmlEvent::Characters(s)) => {
                $parser(s)
            },
            Some(XmlEvent::EndElement { .. }) => return Ok(<_>::default()),
            Some(elem) => return Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $expected,
                found: format!("{elem:?}"),
            })),
            _ => return Err(DecodeError::from(XerDecodeErrorKind::EndOfXmlInput {}))
        };
        $this.next()?;
        value
    }};
}

type Reader<'a> = EventReader<BufReader<&'a [u8]>>;

pub struct Decoder<'a> {
    reader: Reader<'a>,
    next: Option<XmlEvent>,
}

impl<'a> Decoder<'a> {
    pub fn new(input: &'a [u8]) -> Result<Self, <Decoder as crate::de::Decoder>::Error> {
        let mut reader = ParserConfig::default().create_reader(BufReader::new(input));
        let next = reader.next().ok();
        check_prolog(&next)?;
        let mut decoder = Self {
            reader,
            next
        };
        decoder.next()?;
        Ok(decoder)
    }

    pub fn next(&mut self) -> Result<XmlEvent, DecodeError> {
        let mut next = match self.reader.next() {
            Ok(XmlEvent::EndDocument) => None,
            Ok(evt) => Some(evt),
            Err(err) => return Err(error!(XmlParser, "{err:?}")),
        };
        std::mem::swap(&mut next, &mut self.next);
        next.ok_or_else(|| XerDecodeErrorKind::EndOfXmlInput {}.into())
    }

    pub fn peek(&self) -> Option<&XmlEvent> {
        self.next.as_ref()
    }
}

fn check_prolog(next: &Option<XmlEvent>) -> Result<(), DecodeError> {
    if let Some(XmlEvent::StartDocument {
        version, encoding, ..
    }) = next
    {
        if version != &XmlVersion::Version10 || encoding != &String::from("UTF-8") {
            return Err(error!(
                SpecViolation,
                r#"§8.2 The XML prolog shall either be empty; or shall consist of [...] <?xml
                version="1.0"
                encoding="UTF-8"?>"#
            ));
        }
    }
    Ok(())
}

impl crate::Decoder for Decoder<'_> {
    type Error = DecodeError;

    fn codec(&self) -> crate::Codec {
        crate::Codec::Xer
    }

    fn decode_any(&mut self) -> Result<crate::types::Any, Self::Error> {
        todo!()
    }

    fn decode_bit_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::BitString, Self::Error> {
        tag!(StartElement, self, BIT_STRING_TYPE_TAG)?;
        let value = value_or_empty!(self, BIT_STRING_TYPE_TAG, parse_bitstring_value, "`1` or `0`")?;
        tag!(EndElement, self, BIT_STRING_TYPE_TAG)?;
        Ok(value)
    }

    fn decode_bool(&mut self, _tag: Tag) -> Result<bool, Self::Error> {
        tag!(StartElement, self, BOOLEAN_TYPE_TAG)?;
        let value = value!(self, parse_boolean_value, "`true` or `false`")?;
        tag!(EndElement, self, BOOLEAN_TYPE_TAG)?;
        Ok(value)
    }

    fn decode_enumerated<E: Enumerated>(&mut self, tag: Tag) -> Result<E, Self::Error> {
        todo!()
    }

    fn decode_integer(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::Integer, Self::Error> {
        todo!()
    }

    fn decode_null(&mut self, tag: Tag) -> Result<(), Self::Error> {
        todo!()
    }

    fn decode_object_identifier(
        &mut self,
        tag: Tag,
    ) -> Result<crate::types::ObjectIdentifier, Self::Error> {
        todo!()
    }

    fn decode_sequence<D, F>(&mut self, tag: Tag, decode_fn: F) -> Result<D, Self::Error>
    where
        D: crate::types::Constructed,
        F: FnOnce(&mut Self) -> Result<D, Self::Error>,
    {
        todo!()
    }

    fn decode_sequence_of<D: Decode>(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<Vec<D>, Self::Error> {
        todo!()
    }

    fn decode_set_of<D: Decode + Ord>(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::SetOf<D>, Self::Error> {
        todo!()
    }

    fn decode_octet_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<Vec<u8>, Self::Error> {
        todo!()
    }

    fn decode_utf8_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::Utf8String, Self::Error> {
        todo!()
    }

    fn decode_visible_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::VisibleString, Self::Error> {
        todo!()
    }

    fn decode_general_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::GeneralString, Self::Error> {
        todo!()
    }

    fn decode_ia5_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::Ia5String, Self::Error> {
        todo!()
    }

    fn decode_printable_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::PrintableString, Self::Error> {
        todo!()
    }

    fn decode_numeric_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::NumericString, Self::Error> {
        todo!()
    }

    fn decode_teletex_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::TeletexString, Self::Error> {
        todo!()
    }

    fn decode_bmp_string(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<crate::types::BmpString, Self::Error> {
        todo!()
    }

    fn decode_explicit_prefix<D: Decode>(&mut self, tag: Tag) -> Result<D, Self::Error> {
        todo!()
    }

    fn decode_utc_time(&mut self, tag: Tag) -> Result<crate::types::UtcTime, Self::Error> {
        todo!()
    }

    fn decode_generalized_time(
        &mut self,
        tag: Tag,
    ) -> Result<crate::types::GeneralizedTime, Self::Error> {
        todo!()
    }

    fn decode_set<FIELDS, SET, D, F>(
        &mut self,
        tag: Tag,
        decode_fn: D,
        field_fn: F,
    ) -> Result<SET, Self::Error>
    where
        SET: Decode + crate::types::Constructed,
        FIELDS: Decode,
        D: Fn(&mut Self, usize, Tag) -> Result<FIELDS, Self::Error>,
        F: FnOnce(Vec<FIELDS>) -> Result<SET, Self::Error>,
    {
        todo!()
    }

    fn decode_choice<D>(&mut self, constraints: Constraints) -> Result<D, Self::Error>
    where
        D: crate::types::DecodeChoice,
    {
        todo!()
    }

    fn decode_optional<D: Decode>(&mut self) -> Result<Option<D>, Self::Error> {
        todo!()
    }

    fn decode_optional_with_tag<D: Decode>(&mut self, tag: Tag) -> Result<Option<D>, Self::Error> {
        todo!()
    }

    fn decode_optional_with_constraints<D: Decode>(
        &mut self,
        constraints: Constraints,
    ) -> Result<Option<D>, Self::Error> {
        todo!()
    }

    fn decode_optional_with_tag_and_constraints<D: Decode>(
        &mut self,
        tag: Tag,
        constraints: Constraints,
    ) -> Result<Option<D>, Self::Error> {
        todo!()
    }

    fn decode_extension_addition_with_constraints<D>(
        &mut self,
        constraints: Constraints,
    ) -> Result<Option<D>, Self::Error>
    where
        D: Decode,
    {
        todo!()
    }

    fn decode_extension_addition_group<D: Decode + crate::types::Constructed>(
        &mut self,
    ) -> Result<Option<D>, Self::Error> {
        todo!()
    }
}

fn parse_boolean_value(val: &str) -> Result<bool, DecodeError> {
    match val {
        v if v == "true" => Ok(true),
        v if v == "false" => Ok(false),
        _ => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
            needed: "`true` or `false`",
            found: format!("{val:?}"),
        })),
    }
}

fn parse_bitstring_value(val: &str) -> Result<BitString, DecodeError> {
    // TODO: Add support for X.680 §22.9 XMLIdentifierLists
    if !val.chars().all(|c| c == '1' || c == '0' || c.is_whitespace()) {
        return Err(error!(SpecViolation, r#"§12.11 An "xmlbstring" shall consist of an arbitrary number (possibly zero) of zeros, ones or white-space"#))
    }
    Ok(BitString::from_iter(val.chars().filter_map(|c| {
        (c == '1').then(|| true).or((c == '0').then(|| false))
    })))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! decode_test_1 {
        ($suite:ident, $method:ident, $xml:literal, $expected:expr) => {
            #[test]
            fn $suite() {
                let mut decoder = Decoder::new($xml.as_bytes()).unwrap();
                assert_eq!(
                    crate::Decoder::$method(&mut decoder, Tag::CHOICE).unwrap(),
                    $expected
                )
            }
        };
    }

    macro_rules! decode_test_2 {
        ($suite:ident, $method:ident, $xml:literal, $expected:expr) => {
            #[test]
            fn $suite() {
                let mut decoder = Decoder::new($xml.as_bytes()).unwrap();
                assert_eq!(
                    crate::Decoder::$method(&mut decoder, Tag::CHOICE, <_>::default()).unwrap(),
                    $expected
                )
            }
        };
    }

    #[test]
    fn creates_decoder() {
        Decoder::new(
            r#"<?xml version="1.0" encoding="UTF-8"?>
        <Actual>
          <errorCode>
            <local>1</local>
          </errorCode>
          <parameter>
            <BOOLEAN><false/></BOOLEAN>
          </parameter>
        </Actual>"#
                .as_bytes(),
        )
        .unwrap();
    }

    decode_test_1!(boolean, decode_bool, "<BOOLEAN>true</BOOLEAN>", true);
    decode_test_2!(bit_string, decode_bit_string, "<BIT_STRING>1010</BIT_STRING>", bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]);
    decode_test_2!(bit_string_ws, decode_bit_string, "<BIT_STRING>  1   0  1  0  </BIT_STRING>", bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]);
    decode_test_2!(bit_string_empty, decode_bit_string, "<BIT_STRING/>", bitvec::bitvec![u8, bitvec::prelude::Msb0;]);
}
