//! # Encoding XER.
extern crate alloc;

use crate::error::EncodeError;

pub struct Encoder {}

impl Encoder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn finish(self) -> alloc::vec::Vec<u8> {
        todo!()
    }
}

impl crate::Encoder for Encoder {
    type Ok = ();

    type Error = EncodeError;

    fn codec(&self) -> crate::Codec {
        todo!()
    }

    fn encode_any(
        &mut self,
        _tag: crate::Tag,
        _value: &crate::types::Any,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_bool(&mut self, _tag: crate::Tag, _value: bool) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_bit_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::BitStr,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_enumerated<E: crate::types::Enumerated>(
        &mut self,
        _tag: crate::Tag,
        _value: &E,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_object_identifier(
        &mut self,
        _tag: crate::Tag,
        _value: &[u32],
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_integer(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &num_bigint::BigInt,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_null(&mut self, _tag: crate::Tag) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_octet_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &[u8],
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_general_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::GeneralString,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_utf8_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &str,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_visible_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::VisibleString,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_ia5_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::Ia5String,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_printable_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::PrintableString,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_numeric_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::NumericString,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_teletex_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::TeletexString,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_bmp_string(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &crate::types::BmpString,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_generalized_time(
        &mut self,
        _tag: crate::Tag,
        _value: &crate::types::GeneralizedTime,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_utc_time(
        &mut self,
        _tag: crate::Tag,
        _value: &crate::types::UtcTime,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_explicit_prefix<V: crate::Encode>(
        &mut self,
        _tag: crate::Tag,
        _value: &V,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_sequence<C, F>(
        &mut self,
        _tag: crate::Tag,
        _encoder_scope: F,
    ) -> Result<Self::Ok, Self::Error>
    where
        C: crate::types::Constructed,
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        todo!()
    }

    fn encode_sequence_of<E: crate::Encode>(
        &mut self,
        _tag: crate::Tag,
        _value: &[E],
        _constraints: crate::types::Constraints,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_set<C, F>(&mut self, _tag: crate::Tag, _value: F) -> Result<Self::Ok, Self::Error>
    where
        C: crate::types::Constructed,
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        todo!()
    }

    fn encode_set_of<E: crate::Encode>(
        &mut self,
        _tag: crate::Tag,
        _value: &crate::types::SetOf<E>,
        _constraints: crate::types::Constraints,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_some<E: crate::Encode>(&mut self, _value: &E) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_some_with_tag_and_constraints<E: crate::Encode>(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: &E,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_none<E: crate::Encode>(&mut self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_none_with_tag(&mut self, _tag: crate::Tag) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_choice<E: crate::Encode + crate::types::Choice>(
        &mut self,
        _constraints: crate::types::Constraints,
        _tag: crate::Tag,
        _identifier: &'static str,
        _encode_fn: impl FnOnce(&mut Self) -> Result<crate::Tag, Self::Error>,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_extension_addition<E: crate::Encode>(
        &mut self,
        _tag: crate::Tag,
        _constraints: crate::types::Constraints,
        _value: E,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn encode_extension_addition_group<E>(
        &mut self,
        _value: Option<&E>,
    ) -> Result<Self::Ok, Self::Error>
    where
        E: crate::Encode + crate::types::Constructed,
    {
        todo!()
    }
}
