//! # Decoding XER
extern crate alloc;

use xml_no_std::{common::XmlVersion, reader::XmlEvent, ParserConfig};

use crate::{
    error::*,
    types::{fields::Fields, *},
    xer::BOOLEAN_TRUE_TAG,
    Decode,
};

use self::fields::Field;

use super::{BOOLEAN_FALSE_TAG, BOOLEAN_TYPE_TAG};

const OPTIONAL_ITEM_NOT_PRESENT: &str = "§_NOT_PRESENT_§";

macro_rules! error {
    ($kind:ident, $($arg:tt)*) => {
        DecodeError::from(XerDecodeErrorKind::$kind {
            details: alloc::format!($($arg)*)
        })
    };
    ($kind:ident) => {
        DecodeError::from(XerDecodeErrorKind::$kind { })
    };
}

macro_rules! tag {
    ($event:ident, $this:ident, $tag:expr) => {
        match $this.next_element() {
            Some(XmlEvent::$event { name, .. }) => {
                if name.local_name.as_str() == $tag {
                    Ok(())
                } else {
                    Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                        needed: $tag,
                        found: alloc::format!("{name:?}"),
                    }))
                }
            }
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $tag,
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        }
    };
    ($event:ident, $this:ident) => {
        match $this.next_element() {
            Some(XmlEvent::$event { .. }) => Ok(()),
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "StartElement or EndElement",
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        }
    };
}

macro_rules! decode_string {
    ($this:ident, $tryfrom:path, $tag:path, $needed:literal) => {{
        let value = match $this.next_element() {
            Some(XmlEvent::Characters(value)) => $tryfrom(value).map_err(|e| {
                DecodeError::string_conversion_failed(
                    $tag,
                    alloc::format!("Error transforming string: {e:?}"),
                    crate::Codec::Xer,
                )
            }),
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $needed,
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, $this)?;
        value
    }};
}

macro_rules! decode_time {
    ($this:ident, $decode_fn:path) => {{
        tag!(StartElement, $this)?;
        let value = match $this.next_element() {
            Some(XmlEvent::Characters(value)) => $decode_fn(value),
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "Time value",
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, $this)?;
        value
    }};
}

macro_rules! value_or_empty {
    ($this:ident, $tag:expr, $parser:ident, $expected:expr) => {{
        let value = match $this.peek() {
            Some(XmlEvent::Characters(s)) => $parser(s),
            Some(XmlEvent::EndElement { .. }) => return Ok(<_>::default()),
            Some(elem) => {
                return Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                    needed: $expected,
                    found: alloc::format!("{elem:?}"),
                }))
            }
            _ => return Err(DecodeError::from(XerDecodeErrorKind::EndOfXmlInput {})),
        };
        let _ = $this.next_element();
        value
    }};
}

#[derive(Debug)]
struct XerElement {
    events: alloc::collections::VecDeque<XmlEvent>,
}

impl XerElement {
    pub fn next(&mut self) -> Option<XmlEvent> {
        self.events.pop_front()
    }

    pub fn peek(&self) -> Option<&XmlEvent> {
        self.events.front()
    }
}

pub struct Decoder {
    stack: alloc::vec::Vec<XerElement>,
}

impl Decoder {
    pub fn new(input: &[u8]) -> Result<Self, <Decoder as crate::de::Decoder>::Error> {
        let mut reader = ParserConfig::default().create_reader(input.into_iter());
        let next = reader.next().map_err(|e| error!(XmlParser, "{e:?}"))?;
        check_prolog(&next)?;
        let mut elements = alloc::collections::VecDeque::new();
        'read_xml: loop {
            let next = reader.next().map_err(|e| error!(XmlParser, "{e:?}"))?;
            if next == XmlEvent::EndDocument {
                break 'read_xml;
            } else {
                elements.push_back(next);
            }
        }
        elements.try_into()
    }

    pub fn next_element(&mut self) -> Option<XmlEvent> {
        if let Some(mut elem) = self.stack.pop() {
            let event = elem.next();
            if !elem.events.is_empty() {
                self.stack.push(elem);
            }
            event
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&XmlEvent> {
        self.stack.last().and_then(XerElement::peek)
    }

    pub fn sort_by_field_tag_order(
        &mut self,
        field_indices: &[(usize, Field)],
    ) -> Result<(), DecodeError> {
        let field_names = field_indices.iter().map(|(_, f)| f.name).collect();
        self.sort_by_field_name_order(field_names)
    }

    pub fn sort_by_field_name_order(
        &mut self,
        field_names: alloc::vec::Vec<&str>,
    ) -> Result<(), DecodeError> {
        let stack = core::mem::take(&mut self.stack);
        let mut reordered = stack.into_iter().try_fold(
            alloc::collections::BTreeMap::<usize, XerElement>::new(),
            |mut acc, elem| {
                let index = match elem.peek() {
                    Some(XmlEvent::StartElement { name, .. }) => field_names
                        .iter()
                        .enumerate()
                        .find_map(|(i, f)| (*f == name.local_name.as_str()).then_some(i))
                        .ok_or_else(|| {
                            XerDecodeErrorKind::XmlTag {
                                needed: name.local_name.clone(),
                                found: "nothing".into(),
                            }
                            .into()
                        }),
                    e => Err(error!(XmlParser, "Expected opening tag, found {e:?}")),
                };
                index.map(|i| {
                    acc.insert(i, elem);
                    acc
                })
            },
        )?;
        for i in (0..field_names.len()).rev() {
            self.stack.push(reordered.remove(&i).unwrap_or(XerElement {
                events: alloc::vec![XmlEvent::Characters(OPTIONAL_ITEM_NOT_PRESENT.into())].into(),
            }))
        }
        Ok(())
    }
}

impl TryFrom<alloc::collections::VecDeque<XmlEvent>> for Decoder {
    type Error = DecodeError;
    fn try_from(value: alloc::collections::VecDeque<XmlEvent>) -> Result<Self, Self::Error> {
        let (mut stack, mut events, mut tag) =
            (alloc::vec![], alloc::collections::VecDeque::new(), None);
        'xml_elements: for evt in value {
            match (&tag, evt) {
                (_, XmlEvent::Whitespace(_)) => continue 'xml_elements,
                (
                    None,
                    XmlEvent::StartElement {
                        name,
                        attributes,
                        namespace,
                    },
                ) => {
                    tag = Some(name.clone());
                    events.push_back(XmlEvent::StartElement {
                        name,
                        attributes,
                        namespace,
                    })
                }
                (None, _) => {
                    continue 'xml_elements;
                }
                (Some(t), XmlEvent::EndElement { name }) => {
                    if &name == t {
                        events.push_back(XmlEvent::EndElement { name });
                        let collected_events: alloc::collections::VecDeque<XmlEvent> =
                            core::mem::take(&mut events);
                        stack.push(XerElement {
                            events: collected_events,
                        });
                        tag = None;
                    } else {
                        events.push_back(XmlEvent::EndElement { name });
                    }
                }
                (Some(_), XmlEvent::EndDocument) => return Err(error!(EndOfXmlInput)),
                (Some(_), event) => events.push_back(event),
            }
        }
        Ok(Self { stack })
    }
}

fn check_prolog(prolog: &XmlEvent) -> Result<(), DecodeError> {
    if let XmlEvent::StartDocument {
        version, encoding, ..
    } = prolog
    {
        if version != &XmlVersion::Version10 || encoding != &alloc::string::String::from("UTF-8") {
            Err(error!(
                SpecViolation,
                r#"§8.2 The XML prolog shall either be empty; or shall consist of [...] <?xml
                version="1.0"
                encoding="UTF-8"?>"#
            ))
        } else {
            Ok(())
        }
    } else {
        Err(error!(XmlParser, "Expected XML prolog, found {:?}", prolog))
    }
}

impl crate::Decoder for Decoder {
    type Error = DecodeError;

    fn codec(&self) -> crate::Codec {
        crate::Codec::Xer
    }

    fn decode_any(&mut self) -> Result<crate::types::Any, Self::Error> {
        todo!()
    }

    fn decode_bit_string(
        &mut self,
        __tag: Tag,
        __constraints: Constraints,
    ) -> Result<crate::types::BitString, Self::Error> {
        tag!(StartElement, self)?;
        let value = value_or_empty!(
            self,
            BIT_STRING_TYPE_TAG,
            parse_bitstring_value,
            "`1` or `0`"
        );
        tag!(EndElement, self)?;
        value
    }

    fn decode_bool(&mut self, __tag: Tag) -> Result<bool, Self::Error> {
        tag!(StartElement, self)?;
        let value = match self.next_element() {
            Some(XmlEvent::StartElement { name, .. }) => {
                if name.local_name.as_str() == BOOLEAN_TRUE_TAG {
                    tag!(EndElement, self, BOOLEAN_TRUE_TAG).map(|_| true)
                } else if name.local_name.as_str() == BOOLEAN_FALSE_TAG {
                    tag!(EndElement, self, BOOLEAN_FALSE_TAG).map(|_| false)
                } else {
                    Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                        needed: "`<true/>` or `<false/>`",
                        found: alloc::format!("{name:?}"),
                    }))
                }
            }
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: BOOLEAN_TYPE_TAG,
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, self)?;
        value
    }

    fn decode_enumerated<E: Enumerated>(&mut self, __tag: Tag) -> Result<E, Self::Error> {
        tag!(StartElement, self)?;
        let value = match self.next_element() {
            Some(XmlEvent::StartElement { name, .. }) => {
                if let Some((_, enumerated)) = [E::VARIANTS, E::EXTENDED_VARIANTS.unwrap_or(&[])]
                    .into_iter()
                    .flatten()
                    .find(|(id, _)| id == &name.local_name.as_str())
                {
                    tag!(EndElement, self).map(|_| enumerated)
                } else {
                    Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                        needed: "enumerated value",
                        found: alloc::format!("{name:?}"),
                    }))
                }
            }
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "enumerated value",
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, self)?;
        value.copied()
    }

    fn decode_integer(
        &mut self,
        __tag: Tag,
        __constraints: Constraints,
    ) -> Result<crate::types::Integer, Self::Error> {
        tag!(StartElement, self)?;
        let value = match self.next_element() {
            Some(XmlEvent::Characters(value)) => {
                if let Ok(int) = value.parse::<i128>() {
                    Ok(crate::types::Integer::from(int))
                } else {
                    Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                        needed: "integer value",
                        found: value,
                    }))
                }
            }
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "integer value",
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, self)?;
        value
    }

    fn decode_null(&mut self, _tag: Tag) -> Result<(), Self::Error> {
        tag!(StartElement, self)?;
        tag!(EndElement, self)?;
        Ok(())
    }

    fn decode_object_identifier(
        &mut self,
        _tag: Tag,
    ) -> Result<crate::types::ObjectIdentifier, Self::Error> {
        todo!()
    }

    fn decode_sequence<D, F>(&mut self, __tag: Tag, decode_fn: F) -> Result<D, Self::Error>
    where
        D: crate::types::Constructed,
        F: FnOnce(&mut Self) -> Result<D, Self::Error>,
    {
        tag!(StartElement, self)?;
        let field_names = [D::FIELDS, D::EXTENDED_FIELDS.unwrap_or(Fields::empty())]
            .iter()
            .flat_map(|f| f.iter())
            .map(|f| f.name)
            .collect::<alloc::vec::Vec<&str>>();
        let events = self
            .stack
            .pop()
            .ok_or_else(|| error!(EndOfXmlInput))?
            .events;
        let mut sequence_decoder = Decoder::try_from(events)?;
        sequence_decoder.sort_by_field_name_order(field_names)?;
        (decode_fn)(&mut sequence_decoder)
    }

    fn decode_sequence_of<D: Decode>(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<alloc::vec::Vec<D>, Self::Error> {
        decode_sequence_or_set_items(self)
    }

    fn decode_set_of<D: Decode + Ord>(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::SetOf<D>, Self::Error> {
        let items = decode_sequence_or_set_items(self)?;
        Ok(items.into_iter().collect())
    }

    fn decode_octet_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<alloc::vec::Vec<u8>, Self::Error> {
        todo!()
    }

    fn decode_utf8_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::Utf8String, Self::Error> {
        tag!(StartElement, self)?;
        let value = match self.next_element() {
            Some(XmlEvent::Characters(value)) => Ok(value),
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "UTF8 string value",
                found: alloc::format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, self)?;
        value
    }

    fn decode_visible_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::VisibleString, Self::Error> {
        decode_string!(
            self,
            crate::types::VisibleString::try_from,
            Tag::VISIBLE_STRING,
            "VisibleString value"
        )
    }

    fn decode_general_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::GeneralString, Self::Error> {
        decode_string!(
            self,
            crate::types::GeneralString::try_from,
            Tag::GENERAL_STRING,
            "GeneralString value"
        )
    }

    fn decode_ia5_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::Ia5String, Self::Error> {
        decode_string!(
            self,
            crate::types::Ia5String::try_from,
            Tag::IA5_STRING,
            "IA5String value"
        )
    }

    fn decode_printable_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::PrintableString, Self::Error> {
        decode_string!(
            self,
            crate::types::PrintableString::try_from,
            Tag::PRINTABLE_STRING,
            "PrintableString value"
        )
    }

    fn decode_numeric_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::NumericString, Self::Error> {
        decode_string!(
            self,
            crate::types::NumericString::try_from,
            Tag::NUMERIC_STRING,
            "NumericString value"
        )
    }

    fn decode_teletex_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::TeletexString, Self::Error> {
        todo!()
    }

    fn decode_bmp_string(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<crate::types::BmpString, Self::Error> {
        decode_string!(
            self,
            crate::types::BmpString::try_from,
            Tag::BMP_STRING,
            "BMP String value"
        )
    }

    fn decode_explicit_prefix<D: Decode>(&mut self, _tag: Tag) -> Result<D, Self::Error> {
        D::decode(self)
    }

    fn decode_utc_time(&mut self, _tag: Tag) -> Result<crate::types::UtcTime, Self::Error> {
        decode_time!(self, crate::ber::de::Decoder::parse_any_utc_time_string)
    }

    fn decode_generalized_time(
        &mut self,
        _tag: Tag,
    ) -> Result<crate::types::GeneralizedTime, Self::Error> {
        decode_time!(
            self,
            crate::ber::de::Decoder::parse_any_generalized_time_string
        )
    }

    fn decode_set<FIELDS, SET, D, F>(
        &mut self,
        __tag: Tag,
        decode_fn: D,
        field_fn: F,
    ) -> Result<SET, Self::Error>
    where
        SET: Decode + crate::types::Constructed,
        FIELDS: Decode,
        D: Fn(&mut Self, usize, Tag) -> Result<FIELDS, Self::Error>,
        F: FnOnce(alloc::vec::Vec<FIELDS>) -> Result<SET, Self::Error>,
    {
        tag!(StartElement, self)?;
        let events = self
            .stack
            .pop()
            .ok_or_else(|| error!(EndOfXmlInput))?
            .events;
        let mut field_indices = SET::FIELDS
            .iter()
            .enumerate()
            .collect::<alloc::vec::Vec<_>>();
        let mut fields = alloc::vec![];
        field_indices
            .sort_by(|(_, a), (_, b)| a.tag_tree.smallest_tag().cmp(&b.tag_tree.smallest_tag()));
        let mut sequence_decoder = Decoder::try_from(events)?;
        sequence_decoder.sort_by_field_tag_order(&field_indices)?;
        for (index, field) in field_indices.into_iter() {
            fields.push((decode_fn)(&mut sequence_decoder, index, field.tag)?);
        }

        for (index, field) in SET::EXTENDED_FIELDS
            .iter()
            .flat_map(|fields| fields.iter())
            .enumerate()
        {
            fields.push((decode_fn)(
                &mut sequence_decoder,
                index + SET::FIELDS.len(),
                field.tag,
            )?)
        }

        (field_fn)(fields)
    }

    fn decode_choice<D>(&mut self, _constraints: Constraints) -> Result<D, Self::Error>
    where
        D: crate::types::DecodeChoice,
    {
        tag!(StartElement, self)?;
        match self.peek() {
            Some(XmlEvent::StartElement { name, .. }) => {
                let tag = D::IDENTIFIERS
                    .iter()
                    .enumerate()
                    .find(|(_, id)| 
                        id.eq_ignore_ascii_case(&name.local_name)
                    )
                    .and_then(|(i, _)| {
                        variants::Variants::from_slice(
                            &[D::VARIANTS, D::EXTENDED_VARIANTS.unwrap_or(&[])].concat(),
                        )
                        .get(i)
                        .cloned()
                    })
                    .unwrap_or(Tag::EOC);
                let events = self
                    .stack
                    .pop()
                    .ok_or_else(|| error!(EndOfXmlInput))?
                    .events;
                let mut variant_decoder = Decoder::try_from(events)?;
                D::from_tag(&mut variant_decoder, tag)
            }
            elem => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "Start element of choice option",
                found: alloc::format!("{elem:?}"),
            })),
        }
    }

    fn decode_optional<D: Decode>(&mut self) -> Result<Option<D>, Self::Error> {
        Ok(D::decode(self).ok())
    }

    fn decode_optional_with_tag<D: Decode>(&mut self, _tag: Tag) -> Result<Option<D>, Self::Error> {
        self.decode_optional()
    }

    fn decode_optional_with_constraints<D: Decode>(
        &mut self,
        _constraints: Constraints,
    ) -> Result<Option<D>, Self::Error> {
        todo!()
    }

    fn decode_optional_with_tag_and_constraints<D: Decode>(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
    ) -> Result<Option<D>, Self::Error> {
        todo!()
    }

    fn decode_extension_addition_with_constraints<D>(
        &mut self,
        _constraints: Constraints,
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

fn parse_bitstring_value(val: &str) -> Result<BitString, DecodeError> {
    // TODO: Add support for X.680 §22.9 XMLIdentifierLists
    if !val
        .chars()
        .all(|c| c == '1' || c == '0' || c.is_whitespace())
    {
        return Err(error!(
            SpecViolation,
            r#"§12.11 An "xmlbstring" shall consist of an arbitrary number (possibly zero) of zeros, ones or white-space"#
        ));
    }
    Ok(BitString::from_iter(val.chars().filter_map(|c| {
        (c == '1').then_some(true).or((c == '0').then_some(false))
    })))
}

fn decode_sequence_or_set_items<D: Decode>(
    decoder: &mut Decoder,
) -> Result<alloc::vec::Vec<D>, DecodeError> {
    let _identifier = match decoder.next_element() {
        Some(XmlEvent::StartElement { name, .. }) => Ok(name),
        elem => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
            needed: "StartElement of SEQUENCE OF",
            found: alloc::format!("{elem:?}"),
        })),
    }?;

    let mut inner_decoder: Decoder = decoder
        .stack
        .pop()
        .ok_or_else(|| error!(EndOfXmlInput))?
        .events
        .try_into()?;

    let mut items = alloc::vec::Vec::new();
    while inner_decoder.peek().is_some() {
        items.push(D::decode(&mut inner_decoder)?);
    }
    items.reverse();

    Ok(items)
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

    decode_test_1!(
        boolean_true,
        decode_bool,
        "<BOOLEAN><true/></BOOLEAN>",
        true
    );
    decode_test_1!(
        boolean_false,
        decode_bool,
        "<BOOLEAN><false/></BOOLEAN>",
        false
    );
    decode_test_2!(
        bit_string,
        decode_bit_string,
        "<BIT_STRING>1010</BIT_STRING>",
        bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]
    );
    decode_test_2!(
        bit_string_ws,
        decode_bit_string,
        "<BIT_STRING>  1   0  1  0  </BIT_STRING>",
        bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]
    );
    decode_test_2!(
        bit_string_empty,
        decode_bit_string,
        "<BIT_STRING/>",
        bitvec::bitvec![u8, bitvec::prelude::Msb0;]
    );

    #[derive(AsnType, Decode, Debug, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(crate_root = "crate")]
    struct TestTypeA {
        wine: bool,
        grappa: BitString,
    }

    #[derive(AsnType, Decode, Debug, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(crate_root = "crate")]
    struct TestTypeParent {
        sinmin: bool,
        nested: TestTypeA,
    }

    #[test]
    fn sequence() {
        let mut decoder = Decoder::new(
            "<TestTypeA><grappa>1010</grappa><wine><false/></wine></TestTypeA>".as_bytes(),
        )
        .unwrap();
        assert_eq!(
            TestTypeA::decode(&mut decoder).unwrap(),
            TestTypeA {
                wine: false,
                grappa: bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]
            }
        )
    }

    #[test]
    fn sequence_nested() {
        let mut decoder = Decoder::new(
            "<TestTypeParent><nested><grappa>1 11 1 </grappa><wine><false/></wine></nested><sinmin><true/></sinmin></TestTypeParent>".as_bytes(),
        )
        .unwrap();
        assert_eq!(
            TestTypeParent::decode(&mut decoder).unwrap(),
            TestTypeParent {
                nested: TestTypeA {
                    wine: false,
                    grappa: bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 1, 1, 1]
                },
                sinmin: true
            }
        )
    }

    #[derive(AsnType, Debug, Decode, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(crate_root = "crate")]
    #[rasn(set)]
    struct TestSetA {
        wine: bool,
        grappa: BitString,
    }

    #[test]
    fn set() {
        let mut decoder = Decoder::new(
            "<TestTypeA><grappa>1010</grappa><wine><false/></wine></TestTypeA>".as_bytes(),
        )
        .unwrap();
        assert_eq!(
            TestSetA::decode(&mut decoder).unwrap(),
            TestSetA {
                wine: false,
                grappa: bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]
            }
        )
    }

    decode_test_2!(
        positive_int,
        decode_integer,
        "<INTEGER>1283749501626451264</INTEGER>",
        crate::types::Integer::from(1283749501626451264_i128)
    );

    decode_test_2!(
        negative_int,
        decode_integer,
        "<INTEGER>-124142</INTEGER>",
        crate::types::Integer::from(-124142)
    );

    #[derive(AsnType, Decode, Debug, PartialEq, Clone, Copy)]
    #[rasn(enumerated)]
    #[rasn(automatic_tags)]
    #[rasn(crate_root = "crate")]
    enum TestEnum {
        #[rasn(identifier = "option-A")]
        OptionA,
        #[rasn(identifier = "option-B")]
        OptionB,
    }

    #[test]
    fn enumerated() {
        let mut decoder = Decoder::new("<TestEnum><option-B/></TestEnum>".as_bytes()).unwrap();
        assert_eq!(TestEnum::decode(&mut decoder).unwrap(), TestEnum::OptionB)
    }

    #[derive(AsnType, Debug, Decode, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(delegate)]
    #[rasn(crate_root = "crate")]
    struct SeqOfType(SequenceOf<Integer>);

    #[test]
    fn sequence_of() {
        let mut decoder = Decoder::new(
            r#"<SeqOfType>
            <INTEGER>-1</INTEGER>
            <INTEGER>-5</INTEGER>
            <INTEGER>0</INTEGER>
            <INTEGER>55</INTEGER>
            <INTEGER>212424214</INTEGER>
          </SeqOfType>"#
                .as_bytes(),
        )
        .unwrap();
        assert_eq!(
            SeqOfType::decode(&mut decoder).unwrap(),
            SeqOfType(vec![
                Integer::from(-1),
                Integer::from(-5),
                Integer::from(0),
                Integer::from(55),
                Integer::from(212424214)
            ])
        )
    }

    #[derive(AsnType, Debug, Decode, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(delegate)]
    #[rasn(crate_root = "crate")]
    struct NestedSeqOf(SequenceOf<SeqOfType>);

    #[test]
    fn nested_sequence_of() {
        let mut decoder = Decoder::new(
            r#"<NestedSeqOf>
                <SeqOfType>
                    <INTEGER>-1</INTEGER>
                    <INTEGER>-5</INTEGER>
                    <INTEGER>0</INTEGER>
                    <INTEGER>55</INTEGER>
                    <INTEGER>212424214</INTEGER>
                </SeqOfType>
                <SeqOfType>
                    <INTEGER>1</INTEGER>
                    <INTEGER>5</INTEGER>
                    <INTEGER>0</INTEGER>
                    <INTEGER>55</INTEGER>
                    <INTEGER>212424214</INTEGER>
                </SeqOfType>
            </NestedSeqOf>"#
                .as_bytes(),
        )
        .unwrap();
        assert_eq!(
            NestedSeqOf::decode(&mut decoder).unwrap(),
            NestedSeqOf(vec![
                SeqOfType(vec![
                    Integer::from(-1),
                    Integer::from(-5),
                    Integer::from(0),
                    Integer::from(55),
                    Integer::from(212424214)
                ]),
                SeqOfType(vec![
                    Integer::from(1),
                    Integer::from(5),
                    Integer::from(0),
                    Integer::from(55),
                    Integer::from(212424214)
                ])
            ])
        )
    }

    #[derive(AsnType, Debug, Decode, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(delegate)]
    #[rasn(crate_root = "crate")]
    struct SetOfType(crate::types::SetOf<Integer>);

    #[test]
    fn set_of() {
        let mut decoder = Decoder::new(
            r#"<SeqOfType>
            <INTEGER>-1</INTEGER>
            <INTEGER>-5</INTEGER>
            <INTEGER>0</INTEGER>
            <INTEGER>55</INTEGER>
            <INTEGER>212424214</INTEGER>
          </SeqOfType>"#
                .as_bytes(),
        )
        .unwrap();
        let expected: crate::types::SetOf<Integer> = [
            Integer::from(-1),
            Integer::from(-5),
            Integer::from(0),
            Integer::from(55),
            Integer::from(212424214),
        ]
        .into_iter()
        .collect();

        assert_eq!(
            SetOfType::decode(&mut decoder).unwrap(),
            SetOfType(expected)
        )
    }

    #[test]
    fn generalized_time() {
        let mut decoder =
            Decoder::new(r#"<TimeType>20001231235959.999+0000</TimeType>"#.as_bytes()).unwrap();

        assert_eq!(
            crate::types::GeneralizedTime::decode(&mut decoder).unwrap(),
            GeneralizedTime::from(
                chrono::NaiveDate::from_ymd_opt(2000, 12, 31)
                    .unwrap()
                    .and_hms_milli_opt(23, 59, 59, 999)
                    .unwrap()
                    .and_utc()
            )
        )
    }

    #[test]
    fn utc_time() {
        let mut decoder = Decoder::new(r#"<TimeType>991231235900Z</TimeType>"#.as_bytes()).unwrap();

        assert_eq!(
            crate::types::UtcTime::decode(&mut decoder).unwrap(),
            UtcTime::from(
                chrono::NaiveDate::from_ymd_opt(1999, 12, 31)
                    .unwrap()
                    .and_hms_opt(23, 59, 0)
                    .unwrap()
                    .and_utc()
            )
        )
    }

    #[derive(AsnType, Debug, Decode, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(choice)]
    #[rasn(crate_root = "crate")]
    enum ChoiceType {
        #[rasn(identifier = "int")]
        Int(u8),
        #[rasn(identifier = "bool")]
        Bool(bool),
    }

    #[test]
    fn choice() {
        let mut decoder = Decoder::new(
            r#"<ChoiceType>
            <int>5</int>
          </ChoiceType>"#
                .as_bytes(),
        )
        .unwrap();

        assert_eq!(
            ChoiceType::decode(&mut decoder).unwrap(),
            ChoiceType::Int(5)
        )
    }

    #[test]
    fn sequence_of_choices() {
        let mut decoder = Decoder::new(
            r#"
        <SEQUENCE_OF>
            <ChoiceType>
                <int>5</int>
            </ChoiceType>
            <ChoiceType>
                <bool><false/></bool>
            </ChoiceType>
        </SEQUENCE_OF>"#
                .as_bytes(),
        )
        .unwrap();

        assert_eq!(
            SequenceOf::<ChoiceType>::decode(&mut decoder).unwrap(),
            vec![ChoiceType::Int(5), ChoiceType::Bool(false)]
        )
    }

    #[derive(AsnType, Debug, Decode, PartialEq)]
    #[rasn(automatic_tags)]
    #[rasn(crate_root = "crate")]
    struct OptionalTest {
        wine: Option<bool>,
        grappa: BitString,
    }

    #[test]
    fn optional_present() {
        let mut decoder = Decoder::new(
            "<TestTypeA><grappa>1010</grappa><wine><false/></wine></TestTypeA>".as_bytes(),
        )
        .unwrap();
        assert_eq!(
            OptionalTest::decode(&mut decoder).unwrap(),
            OptionalTest {
                wine: Some(false),
                grappa: bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]
            }
        )
    }

    #[test]
    fn optional_absent() {
        let mut decoder = Decoder::new(
            "<TestTypeA><grappa>1010</grappa></TestTypeA>".as_bytes(),
        )
        .unwrap();
        assert_eq!(
            OptionalTest::decode(&mut decoder).unwrap(),
            OptionalTest {
                wine: None,
                grappa: bitvec::bitvec![u8, bitvec::prelude::Msb0; 1, 0, 1, 0]
            }
        )
    }
}
