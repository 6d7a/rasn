//! # Decoding XER

use std::{collections::BTreeMap, io::BufReader};

use alloc::collections::VecDeque;
use xml::{common::XmlVersion, reader::XmlEvent, ParserConfig};

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
            details: format!($($arg)*)
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
                        found: format!("{name:?}"),
                    }))
                }
            }
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $tag,
                found: format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        }
    };
    ($event:ident, $this:ident) => {
        match $this.next_element() {
            Some(XmlEvent::$event { .. }) => Ok(()),
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: "StartElement or EndElement",
                found: format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        }
    };
}

macro_rules! value {
    ($this:ident, $parser:ident, $expected:expr) => {{
        match $this.next_element() {
            Some(XmlEvent::Characters(s)) => $parser(&s),
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: $expected,
                found: format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        }
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
                    found: format!("{elem:?}"),
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
    events: VecDeque<XmlEvent>,
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
    stack: Vec<XerElement>,
}

impl Decoder {
    pub fn new(input: &[u8]) -> Result<Self, <Decoder as crate::de::Decoder>::Error> {
        let mut reader = ParserConfig::default().create_reader(BufReader::new(input));
        let next = reader.next().map_err(|e| error!(XmlParser, "{e:?}"))?;
        check_prolog(&next)?;
        let mut elements = VecDeque::new();
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

    pub fn sort_by_field_tag_order(&mut self, field_indices: &[(usize, Field)]) -> Result<(), DecodeError> {
        let field_names = field_indices.iter().map(|(_, f)| f.name).collect();
        self.sort_by_field_name_order(field_names)
    }

    pub fn sort_by_field_name_order(&mut self, field_names: Vec<&str>) -> Result<(), DecodeError> {
        let stack = std::mem::take(&mut self.stack);
        let mut reordered =
            stack
                .into_iter()
                .try_fold(BTreeMap::<usize, XerElement>::new(), |mut acc, elem| {
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
                })?;
        for i in (0..field_names.len()).rev() {
            self.stack.push(reordered.remove(&i).unwrap_or(XerElement {
                events: vec![XmlEvent::Characters(OPTIONAL_ITEM_NOT_PRESENT.into())].into(),
            }))
        }
        Ok(())
    }
}

impl TryFrom<VecDeque<XmlEvent>> for Decoder {
    type Error = DecodeError;
    fn try_from(value: VecDeque<XmlEvent>) -> Result<Self, Self::Error> {
        let (mut stack, mut events, mut tag) = (vec![], VecDeque::new(), None);
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
                        let collected_events: VecDeque<XmlEvent> = std::mem::take(&mut events);
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
        if version != &XmlVersion::Version10 || encoding != &String::from("UTF-8") {
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
        _tag: Tag,
        _constraints: Constraints,
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

    fn decode_bool(&mut self, _tag: Tag) -> Result<bool, Self::Error> {
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
                        found: format!("{name:?}"),
                    }))
                }
            }
            Some(elem) => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
                needed: BOOLEAN_TYPE_TAG,
                found: format!("{elem:?}"),
            })),
            None => Err(error!(EndOfXmlInput)),
        };
        tag!(EndElement, self)?;
        value
    }

    fn decode_enumerated<E: Enumerated>(&mut self, _tag: Tag) -> Result<E, Self::Error> {
        todo!()
    }

    fn decode_integer(
        &mut self,
        _tag: Tag,
        _constraints: Constraints,
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

    fn decode_sequence<D, F>(&mut self, _tag: Tag, decode_fn: F) -> Result<D, Self::Error>
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
            fields.push((decode_fn)(&mut sequence_decoder, index + SET::FIELDS.len(), field.tag)?)
        }

        (field_fn)(fields)
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
        v if v == "<true/>" => Ok(true),
        v if v == "<false/>" => Ok(false),
        _ => Err(DecodeError::from(XerDecodeErrorKind::XmlTypeMismatch {
            needed: "`true` or `false`",
            found: format!("{val:?}"),
        })),
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
}
