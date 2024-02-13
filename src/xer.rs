extern crate alloc;

pub mod de;
pub mod enc;

pub(self) const BIT_STRING_TYPE_TAG: &str = "BIT_STRING";
pub(self) const BOOLEAN_TYPE_TAG: &str = "BOOLEAN";
pub(self) const BOOLEAN_TRUE_TAG: &str = "true";
pub(self) const BOOLEAN_FALSE_TAG: &str = "false";
pub(self) const CHOICE_TYPE_TAG: &str = "CHOICE";
pub(self) const DURATION_TYPE_TAG: &str = "DURATION";
pub(self) const EMBEDDED_PDV_TYPE_TAG: &str = "SEQUENCE";
pub(self) const ENUMERATED_TYPE_TAG: &str = "ENUMERATED";
pub(self) const EXTERNAL_TYPE_TAG: &str = "SEQUENCE";
pub(self) const INSTANCE_OF_TYPE_TAG: &str = "SEQUENCE";
pub(self) const INTEGER_TYPE_TAG: &str = "INTEGER";
pub(self) const NULL_TYPE_TAG: &str = "NULL";
pub(self) const OBJECT_IDENTIFIER_TYPE_TAG: &str = "OBJECT_IDENTIFIER";
pub(self) const OCTET_STRING_TYPE_TAG: &str = "OCTET_STRING";
pub(self) const SEQUENCE_TYPE_TAG: &str = "SEQUENCE";
pub(self) const SEQUENCE_OF_TYPE_TAG: &str = "SEQUENCE_OF";
pub(self) const SET_TYPE_TAG: &str = "SET";
pub(self) const SET_OF_TYPE_TAG: &str = "SET_OF";

/// Attempts to decode `T` from `input` using XER.
/// # Errors
/// Returns error specific to XER decoder if decoding is not possible.
pub fn decode<'de, T: crate::Decode>(input: &'de [u8]) -> Result<T, crate::error::DecodeError> {
    T::decode(&mut de::Decoder::new(input)?)
}

/// Attempts to encode `value` to XER.
/// # Errors
/// Returns error specific to XER encoder if encoding is not possible.
pub fn encode<T: crate::Encode>(value: &T) -> Result<alloc::vec::Vec<u8>, crate::error::EncodeError> {
    let mut encoder = enc::Encoder::new();
    value.encode(&mut encoder)?;
    Ok(encoder.finish())
}

#[cfg(test)]
mod tests {
    // macro_rules! round_trip_xer {
    //     ($typ:ty, $value:expr, $expected:expr) => {{
    //         let value: $typ = $value;
    //         let expected: &'static str = $expected;
    //         let actual_encoding = crate::xer::encode(&value).unwrap();

    //         pretty_assertions::assert_eq!(expected, &*actual_encoding);

    //         let decoded_value: $typ = crate::xer::decode(&actual_encoding).unwrap();

    //         pretty_assertions::assert_eq!(value, decoded_value);
    //     }};
    // }

    // macro_rules! round_trip_string_type {
    //     ($typ:ty) => {{
    //         let string = String::from(" 1234567890");
    //         let expected: &'static str = "\" 1234567890\"";
    //         let value: $typ = string.try_into().unwrap();
    //         let actual_encoding = crate::xer::encode(&value).unwrap();

    //         pretty_assertions::assert_eq!(expected, &actual_encoding);

    //         let decoded_value: $typ = crate::xer::decode(&actual_encoding).unwrap();

    //         pretty_assertions::assert_eq!(value, decoded_value);
    //     }};
    // }

    // use crate::prelude::*;

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(automatic_tags)]
    // #[rasn(crate_root = "crate")]
    // #[non_exhaustive]
    // struct TestTypeA {
    //     #[rasn(value("0..3", extensible))]
    //     juice: Integer,
    //     wine: Inner,
    //     #[rasn(extension_addition)]
    //     grappa: BitString,
    // }

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(choice, automatic_tags)]
    // #[rasn(crate_root = "crate")]
    // enum Inner {
    //     #[rasn(value("0..3"))]
    //     Wine(u8),
    // }

    // #[derive(AsnType, Decode, Encode, Debug, Clone, Copy, PartialEq)]
    // #[rasn(automatic_tags, enumerated)]
    // #[rasn(crate_root = "crate")]
    // enum SimpleEnum {
    //     Test1 = 5,
    //     Test2 = 2,
    // }

    // #[derive(AsnType, Decode, Encode, Debug, Clone, Copy, PartialEq)]
    // #[rasn(automatic_tags, enumerated)]
    // #[rasn(crate_root = "crate")]
    // #[non_exhaustive]
    // enum ExtEnum {
    //     Test1 = 5,
    //     Test2 = 2,
    //     #[rasn(extension_addition)]
    //     Test3 = -1,
    // }

    // #[derive(AsnType, Decode, Encode, Debug, Clone, PartialEq, Ord, Eq, PartialOrd)]
    // #[rasn(automatic_tags, choice)]
    // #[rasn(crate_root = "crate")]
    // enum SimpleChoice {
    //     Test1(u8),
    //     #[rasn(size("0..3"))]
    //     Test2(Utf8String),
    // }

    // #[derive(AsnType, Decode, Encode, Debug, Clone, PartialEq)]
    // #[rasn(automatic_tags, choice)]
    // #[rasn(crate_root = "crate")]
    // #[non_exhaustive]
    // enum ExtChoice {
    //     Test1(u8),
    //     #[rasn(size("0..3"))]
    //     Test2(Utf8String),
    //     #[rasn(extension_addition)]
    //     Test3(bool),
    // }

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(automatic_tags)]
    // #[rasn(crate_root = "crate")]
    // #[non_exhaustive]
    // struct Very {
    //     #[rasn(extension_addition)]
    //     a: Option<Nested>,
    // }

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(automatic_tags)]
    // #[rasn(crate_root = "crate")]
    // struct Nested {
    //     very: Option<Struct>,
    //     nested: Option<bool>,
    // }

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(automatic_tags)]
    // #[rasn(crate_root = "crate")]
    // struct Struct {
    //     strct: Option<u8>,
    // }

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(crate_root = "crate", delegate, size("3", extensible))]
    // struct ConstrainedOctetString(pub OctetString);

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(crate_root = "crate", delegate, value("-5..=5", extensible))]
    // struct ConstrainedInt(pub Integer);

    // #[derive(AsnType, Decode, Encode, Debug, PartialEq)]
    // #[rasn(crate_root = "crate", delegate, size("3", extensible))]
    // struct ConstrainedBitString(pub BitString);

    // #[test]
    // fn bool() {
    //     round_trip_xer!(bool, true, "true");
    //     round_trip_xer!(bool, false, "false");
    // }

    // #[test]
    // fn integer() {
    //     round_trip_xer!(u8, 1, "1");
    //     round_trip_xer!(i8, -1, "-1");
    //     round_trip_xer!(u16, 0, "0");
    //     round_trip_xer!(i16, -14321, "-14321");
    //     round_trip_xer!(i64, -1213428598524996264, "-1213428598524996264");
    //     round_trip_xer!(Integer, 1.into(), "1");
    //     round_trip_xer!(Integer, (-1235352).into(), "-1235352");
    //     round_trip_xer!(ConstrainedInt, ConstrainedInt(1.into()), "1");
    // }

    // #[test]
    // fn bit_string() {
    //     round_trip_xer!(
    //         BitString,
    //         BitString::from_iter([true, false].into_iter()),
    //         "\"10\""
    //     );
    //     round_trip_xer!(
    //         ConstrainedBitString,
    //         ConstrainedBitString(BitString::from_iter([true, false, true, true].into_iter())),
    //         "\"1011\""
    //     );
    // }

    // #[test]
    // fn octet_string() {
    //     round_trip_xer!(OctetString, OctetString::from_static(&[1, 255]), "\"01FF\"");
    //     round_trip_xer!(
    //         ConstrainedOctetString,
    //         ConstrainedOctetString(OctetString::from_static(&[1, 255, 0, 254])),
    //         "\"01FF00FE\""
    //     );
    // }

    // #[test]
    // fn object_identifier() {
    //     round_trip_xer!(
    //         ObjectIdentifier,
    //         ObjectIdentifier::from(Oid::JOINT_ISO_ITU_T_DS_NAME_FORM),
    //         "\"2.5.15\""
    //     );
    // }

    // #[test]
    // fn string_types() {
    //     round_trip_string_type!(NumericString);
    //     round_trip_string_type!(GeneralString);
    //     round_trip_string_type!(VisibleString);
    //     round_trip_string_type!(UniversalString);
    //     round_trip_string_type!(PrintableString);
    //     round_trip_string_type!(Ia5String);
    //     round_trip_string_type!(Utf8String);
    // }

    // #[test]
    // fn enumerated() {
    //     round_trip_xer!(SimpleEnum, SimpleEnum::Test1, "5");
    //     round_trip_xer!(SimpleEnum, SimpleEnum::Test2, "2");
    //     round_trip_xer!(ExtEnum, ExtEnum::Test1, "5");
    //     round_trip_xer!(ExtEnum, ExtEnum::Test2, "2");
    //     round_trip_xer!(ExtEnum, ExtEnum::Test3, "-1");
    // }

    // #[test]
    // fn choice() {
    //     round_trip_xer!(SimpleChoice, SimpleChoice::Test1(3), "{\"Test1\":3}");
    //     round_trip_xer!(
    //         SimpleChoice,
    //         SimpleChoice::Test2("foo".into()),
    //         "{\"Test2\":\"foo\"}"
    //     );
    //     round_trip_xer!(ExtChoice, ExtChoice::Test1(255), "{\"Test1\":255}");
    //     round_trip_xer!(
    //         ExtChoice,
    //         ExtChoice::Test2("bar".into()),
    //         "{\"Test2\":\"bar\"}"
    //     );
    //     round_trip_xer!(ExtChoice, ExtChoice::Test3(true), "{\"Test3\":true}");
    // }

    // #[test]
    // fn sequence_of() {
    //     round_trip_xer!(
    //         SequenceOf<SimpleChoice>,
    //         alloc::alloc::vec::Vec![SimpleChoice::Test1(3)],
    //         "[{\"Test1\":3}]"
    //     );
    //     round_trip_xer!(
    //         SequenceOf<u8>,
    //         alloc::alloc::vec::Vec![1, 2, 3, 4, 5, 5, 3],
    //         "[1,2,3,4,5,5,3]"
    //     );
    //     round_trip_xer!(SequenceOf<bool>, alloc::alloc::vec::Vec![], "[]");
    // }

    // #[test]
    // fn set_of() {
    //     round_trip_xer!(
    //         SetOf<SimpleChoice>,
    //         alloc::alloc::vec::Vec![SimpleChoice::Test1(3)].into_iter().collect(),
    //         "[{\"Test1\":3}]"
    //     );
    //     round_trip_xer!(
    //         SetOf<u8>,
    //         alloc::alloc::vec::Vec![1, 2, 3, 4, 5].into_iter().collect(),
    //         "[1,2,3,4,5]"
    //     );
    //     round_trip_xer!(SetOf<bool>, alloc::alloc::vec::Vec![].into_iter().collect(), "[]");
    // }

    // #[test]
    // fn seqence() {
    //     round_trip_xer!(
    //         TestTypeA,
    //         TestTypeA {
    //             juice: 0.into(),
    //             wine: Inner::Wine(4),
    //             grappa: BitString::from_iter([true, false].iter())
    //         },
    //         "{\"juice\":0,\"wine\":{\"Wine\":4},\"grappa\":\"10\"}"
    //     );
    //     round_trip_xer!(
    //         Very,
    //         Very {
    //             a: Some(Nested {
    //                 very: Some(Struct { strct: None }),
    //                 nested: Some(false)
    //             })
    //         },
    //         "{\"a\":{\"very\":{},\"nested\":false}}"
    //     );
    // }
}
