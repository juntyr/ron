//! Value module.

use serde::{
    de::{DeserializeOwned, DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor},
    forward_to_deserialize_any, Deserialize, Serialize,
};
use std::{
    cmp::{Eq, Ordering},
    hash::{Hash, Hasher},
    iter::FromIterator,
    ops::{Index, IndexMut},
};

use crate::de::{Error as RonError, Result};

/// A `Value` to `Value` map.
///
/// This structure either uses a [BTreeMap](std::collections::BTreeMap) or the
/// [IndexMap](indexmap::IndexMap) internally.
/// The latter can be used by enabling the `indexmap` feature. This can be used
/// to preserve the order of the parsed map.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Map(MapInner);

impl Map {
    /// Creates a new, empty `Map`.
    pub fn new() -> Map {
        Default::default()
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if `self.len() == 0`, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    /// Inserts a new element, returning the previous element with this `key` if
    /// there was any.
    pub fn insert(&mut self, key: Value, value: Value) -> Option<Value> {
        self.0.insert(key, value)
    }

    /// Removes an element by its `key`.
    pub fn remove(&mut self, key: &Value) -> Option<Value> {
        self.0.remove(key)
    }

    /// Iterate all key-value pairs.
    pub fn iter(&self) -> impl Iterator<Item = (&Value, &Value)> + DoubleEndedIterator {
        self.0.iter()
    }

    /// Iterate all key-value pairs mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&Value, &mut Value)> + DoubleEndedIterator {
        self.0.iter_mut()
    }

    /// Iterate all keys.
    pub fn keys(&self) -> impl Iterator<Item = &Value> + DoubleEndedIterator {
        self.0.keys()
    }

    /// Iterate all values.
    pub fn values(&self) -> impl Iterator<Item = &Value> + DoubleEndedIterator {
        self.0.values()
    }

    /// Iterate all values mutably.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Value> + DoubleEndedIterator {
        self.0.values_mut()
    }
}

impl FromIterator<(Value, Value)> for Map {
    fn from_iter<T: IntoIterator<Item = (Value, Value)>>(iter: T) -> Self {
        Map(MapInner::from_iter(iter))
    }
}

impl IntoIterator for Map {
    type Item = (Value, Value);
    type IntoIter = <MapInner as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Note: equality is only given if both values and order of values match
impl Eq for Map {}

impl Hash for Map {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.iter().for_each(|x| x.hash(state));
    }
}

impl Index<&Value> for Map {
    type Output = Value;

    fn index(&self, index: &Value) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<&Value> for Map {
    fn index_mut(&mut self, index: &Value) -> &mut Self::Output {
        self.0.get_mut(index).expect("no entry found for key")
    }
}

impl Ord for Map {
    fn cmp(&self, other: &Map) -> Ordering {
        self.iter().cmp(other.iter())
    }
}

/// Note: equality is only given if both values and order of values match
impl PartialEq for Map {
    fn eq(&self, other: &Map) -> bool {
        self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}

impl PartialOrd for Map {
    fn partial_cmp(&self, other: &Map) -> Option<Ordering> {
        self.iter().partial_cmp(other.iter())
    }
}

#[cfg(not(feature = "indexmap"))]
type MapInner = std::collections::BTreeMap<Value, Value>;
#[cfg(feature = "indexmap")]
type MapInner = indexmap::IndexMap<Value, Value>;

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Number {
    F32(F32),
    F64(F64),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    #[cfg(feature = "integer128")]
    I128(i128),
    #[cfg(feature = "integer128")]
    U128(u128),
}

/// A wrapper for `f64`, which guarantees that the inner value
/// is a number and thus implements `Eq`, `Hash` and `Ord`.
#[derive(Copy, Clone, Debug)]
pub struct F64(f64);

impl F64 {
    /// Construct a new `F64`.
    pub fn new(v: f64) -> Self {
        Self(v)
    }

    /// Returns the wrapped `f64`.
    pub fn get(self) -> f64 {
        self.0
    }
}

/// Partial equality comparison
/// In order to be able to use `Number` as a mapping key, NaN floating values
/// wrapped in `F64` are equals to each other. It is not the case for
/// underlying `f64` values itself.
impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_nan() && other.0.is_nan() || self.0 == other.0
    }
}

/// Equality comparison
/// In order to be able to use `Float` as a mapping key, NaN floating values
/// wrapped in `F64` are equals to each other. It is not the case for
/// underlying `f64` values itself.
impl Eq for F64 {}

impl Hash for F64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.to_bits());
    }
}

/// Partial ordering comparison
/// In order to be able to use `Number` as a mapping key, NaN floating values
/// wrapped in `F64` are equals to each other and are less then any other
/// floating value. It is not the case for the underlying `f64` values themselves.
/// ```
/// use ron::value::Number;
/// assert!(Number::new(std::f64::NAN) < Number::new(std::f64::NEG_INFINITY));
/// assert_eq!(Number::new(std::f64::NAN), Number::new(std::f64::NAN));
/// ```
impl PartialOrd for F64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Ordering comparison
/// In order to be able to use `Number` as a mapping key, NaN floating values
/// wrapped in `F64` are equals to each other and are less then any other
/// floating value. It is not the case for the underlying `f64` values themselves.
impl Ord for F64 {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(ord) = self.0.partial_cmp(&other.0) {
            return ord;
        }

        match (self.0.is_nan(), other.0.is_nan()) {
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            (_, _) /*(true, true)*/ => Ordering::Equal,
        }
    }
}

/// A wrapper for `f32`, which guarantees that the inner value
/// is a number and thus implements `Eq`, `Hash` and `Ord`.
#[derive(Copy, Clone, Debug)]
pub struct F32(f32);

impl F32 {
    /// Construct a new `F32`.
    pub fn new(v: f32) -> Self {
        Self(v)
    }

    /// Returns the wrapped `f32`.
    pub fn get(self) -> f32 {
        self.0
    }
}

/// Partial equality comparison
/// In order to be able to use `Number` as a mapping key, NaN floating values
/// wrapped in `F32` are equals to each other. It is not the case for
/// underlying `f32` values itself.
impl PartialEq for F32 {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_nan() && other.0.is_nan() || self.0 == other.0
    }
}

/// Equality comparison
/// In order to be able to use `F32` as a mapping key, NaN floating values
/// wrapped in `F32` are equals to each other. It is not the case for
/// underlying `f32` values itself.
impl Eq for F32 {}

impl Hash for F32 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(self.0.to_bits());
    }
}

/// Partial ordering comparison
/// In order to be able to use `Number` as a mapping key, NaN floating values
/// wrapped in `F32` are equals to each other and are less then any other
/// floating value. It is not the case for the underlying `f32` values themselves.
/// ```
/// use ron::value::Number;
/// assert!(Number::new(std::f32::NAN) < Number::new(std::f32::NEG_INFINITY));
/// assert_eq!(Number::new(std::f32::NAN), Number::new(std::f32::NAN));
/// ```
impl PartialOrd for F32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Ordering comparison
/// In order to be able to use `Number` as a mapping key, NaN floating values
/// wrapped in `F32` are equals to each other and are less then any other
/// floating value. It is not the case for the underlying `f32` values themselves.
impl Ord for F32 {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(ord) = self.0.partial_cmp(&other.0) {
            return ord;
        }

        match (self.0.is_nan(), other.0.is_nan()) {
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            (_, _) /*(true, true)*/ => Ordering::Equal,
        }
    }
}

impl Number {
    /// Construct a new number.
    pub fn new(v: impl Into<Number>) -> Self {
        v.into()
    }
}

impl From<f32> for Number {
    fn from(f: f32) -> Number {
        Number::F32(F32::new(f))
    }
}

impl From<f64> for Number {
    fn from(f: f64) -> Number {
        Number::F64(F64::new(f))
    }
}

impl From<i8> for Number {
    fn from(i: i8) -> Number {
        Number::I8(i)
    }
}

impl From<u8> for Number {
    fn from(u: u8) -> Number {
        Number::U8(u)
    }
}

impl From<i16> for Number {
    fn from(i: i16) -> Number {
        Number::I16(i)
    }
}

impl From<u16> for Number {
    fn from(u: u16) -> Number {
        Number::U16(u)
    }
}

impl From<i32> for Number {
    fn from(i: i32) -> Number {
        Number::I32(i)
    }
}

impl From<u32> for Number {
    fn from(u: u32) -> Number {
        Number::U32(u)
    }
}

impl From<i64> for Number {
    fn from(i: i64) -> Number {
        Number::I64(i)
    }
}

impl From<u64> for Number {
    fn from(u: u64) -> Number {
        Number::U64(u)
    }
}

#[cfg(feature = "integer128")]
impl From<i128> for Number {
    fn from(i: i128) -> Number {
        Number::I128(i)
    }
}

#[cfg(feature = "integer128")]
impl From<u128> for Number {
    fn from(u: u128) -> Number {
        Number::U128(u)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Char(char),
    Map(Map),
    Number(Number),
    Option(Option<Box<Value>>),
    String(String),
    Seq(Vec<Value>),
    Unit,
}

impl Value {
    /// Tries to deserialize this `Value` into `T`.
    pub fn into_rust<T>(self) -> Result<T>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

impl Value {
    fn unexpected(&self) -> serde::de::Unexpected {
        use serde::de::Unexpected;

        match self {
            Self::Bool(b) => Unexpected::Bool(*b),
            Self::Char(c) => Unexpected::Char(*c),
            Self::Map(_) => Unexpected::Map,
            Self::Number(Number::F32(f)) => Unexpected::Float(f.get().into()),
            Self::Number(Number::F64(f)) => Unexpected::Float(f.get()),
            Self::Number(Number::I8(i)) => Unexpected::Signed((*i).into()),
            Self::Number(Number::I16(i)) => Unexpected::Signed((*i).into()),
            Self::Number(Number::I32(i)) => Unexpected::Signed((*i).into()),
            Self::Number(Number::I64(i)) => Unexpected::Signed(*i),
            #[cfg(feature = "integer128")]
            Self::Number(Number::I128(i)) => Unexpected::Signed((*i) as i64),
            Self::Number(Number::U8(u)) => Unexpected::Unsigned((*u).into()),
            Self::Number(Number::U16(u)) => Unexpected::Unsigned((*u).into()),
            Self::Number(Number::U32(u)) => Unexpected::Unsigned((*u).into()),
            Self::Number(Number::U64(u)) => Unexpected::Unsigned(*u),
            #[cfg(feature = "integer128")]
            Self::Number(Number::U128(u)) => Unexpected::Unsigned((*u) as u64),
            Self::Option(_) => Unexpected::Option,
            Self::String(s) => Unexpected::Str(s),
            Self::Seq(_) => Unexpected::Seq,
            Self::Unit => Unexpected::Unit,
        }
    }
}

/// Deserializer implementation for RON `Value`.
/// This does not support enums (because `Value` doesn't store them).
impl<'de> Deserializer<'de> for Value {
    type Error = RonError;

    forward_to_deserialize_any! {
        bool f32 f64 i8 i16 i32 i64 u8 u16 u32 u64
        char str string bytes
        byte_buf option unit seq tuple
        map identifier ignored_any
    }

    #[cfg(feature = "integer128")]
    forward_to_deserialize_any! {
        i128 u128
    }

    fn deserialize_newtype_struct<V>(mut self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if let Value::Map(map) = &mut self {
            if map.len() == 1 {
                let first_key = map.keys().next();

                if let Some(Value::String(key)) = first_key {
                    if key == name {
                        if let Some(inner) = map.remove(&Value::String(String::from(name))) {
                            return Value::Seq(vec![inner]).deserialize_any(visitor);
                        }
                    }
                }
            }
        }

        self.deserialize_any(visitor)
    }

    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if let Value::String(ident) = &self {
            if ident == name {
                return visitor.visit_unit();
            }
        }

        self.deserialize_any(visitor)
    }

    fn deserialize_tuple_struct<V>(
        mut self,
        name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if let Value::Map(map) = &mut self {
            if map.len() == 1 {
                let first_key = map.keys().next();

                if let Some(Value::String(key)) = first_key {
                    if key == name {
                        if let Some(inner) = map.remove(&Value::String(String::from(name))) {
                            return inner.deserialize_any(visitor);
                        }
                    }
                }
            }
        }

        self.deserialize_any(visitor)
    }

    fn deserialize_struct<V>(
        mut self,
        name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if let Value::Map(map) = &mut self {
            if map.len() == 1 {
                let first_key = map.keys().next();

                if let Some(Value::String(key)) = first_key {
                    if key == name {
                        if let Some(inner) = map.remove(&Value::String(String::from(name))) {
                            return inner.deserialize_any(visitor);
                        }
                    }
                }
            }
        }

        self.deserialize_any(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (variant, value) = match self {
            Value::Map(map) => {
                let mut iter = map.into_iter();
                let (variant, value) = match iter.next() {
                    Some(v) => v,
                    None => {
                        return Err(serde::de::Error::invalid_value(
                            serde::de::Unexpected::Map,
                            &"map with a single key",
                        ));
                    }
                };
                // enums are encoded in json as maps with a single key:value pair
                if iter.next().is_some() {
                    return Err(serde::de::Error::invalid_value(
                        serde::de::Unexpected::Map,
                        &"map with a single key",
                    ));
                }
                (variant, Some(value))
            }
            s @ Value::String(_) => (s, None),
            other => {
                return Err(serde::de::Error::invalid_type(
                    other.unexpected(),
                    &"string or map",
                ));
            }
        };

        visitor.visit_enum(EnumRefDeserializer { variant, value })
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self {
            Value::Bool(b) => visitor.visit_bool(b),
            Value::Char(c) => visitor.visit_char(c),
            Value::Map(m) => visitor.visit_map(MapAccessor {
                keys: m.keys().cloned().rev().collect(),
                values: m.values().cloned().rev().collect(),
            }),
            Value::Number(Number::F32(f)) => visitor.visit_f32(f.get()),
            Value::Number(Number::F64(f)) => visitor.visit_f64(f.get()),
            Value::Number(Number::I8(i)) => visitor.visit_i8(i),
            Value::Number(Number::I16(i)) => visitor.visit_i16(i),
            Value::Number(Number::I32(i)) => visitor.visit_i32(i),
            Value::Number(Number::I64(i)) => visitor.visit_i64(i),
            #[cfg(feature = "integer128")]
            Value::Number(Number::I128(i)) => visitor.visit_i128(i),
            Value::Number(Number::U8(u)) => visitor.visit_u8(u),
            Value::Number(Number::U16(u)) => visitor.visit_u16(u),
            Value::Number(Number::U32(u)) => visitor.visit_u32(u),
            Value::Number(Number::U64(u)) => visitor.visit_u64(u),
            #[cfg(feature = "integer128")]
            Value::Number(Number::U128(u)) => visitor.visit_u128(u),
            Value::Option(Some(o)) => visitor.visit_some(*o),
            Value::Option(None) => visitor.visit_none(),
            Value::String(s) => visitor.visit_string(s),
            Value::Seq(mut seq) => {
                seq.reverse();
                visitor.visit_seq(Seq { seq })
            }
            Value::Unit => visitor.visit_unit(),
        }
    }
}

struct MapAccessor {
    keys: Vec<Value>,
    values: Vec<Value>,
}

impl<'de> MapAccess<'de> for MapAccessor {
    type Error = RonError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        // The `Vec` is reversed, so we can pop to get the originally first element
        self.keys
            .pop()
            .map_or(Ok(None), |v| seed.deserialize(v).map(Some))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        // The `Vec` is reversed, so we can pop to get the originally first element
        self.values
            .pop()
            .map(|v| seed.deserialize(v))
            .expect("Contract violation")
    }
}

struct Seq {
    seq: Vec<Value>,
}

impl<'de> SeqAccess<'de> for Seq {
    type Error = RonError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        // The `Vec` is reversed, so we can pop to get the originally first element
        self.seq
            .pop()
            .map_or(Ok(None), |v| seed.deserialize(v).map(Some))
    }
}

struct EnumRefDeserializer {
    variant: Value,
    value: Option<Value>,
}

impl<'de> serde::de::EnumAccess<'de> for EnumRefDeserializer {
    type Error = RonError;
    type Variant = VariantRefDeserializer;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let visitor = VariantRefDeserializer { value: self.value };
        seed.deserialize(self.variant).map(|v| (v, visitor))
    }
}

struct VariantRefDeserializer {
    value: Option<Value>,
}

impl<'de> serde::de::VariantAccess<'de> for VariantRefDeserializer {
    type Error = RonError;

    fn unit_variant(self) -> Result<()> {
        match self.value {
            None => Ok(()),
            Some(other) => Err(serde::de::Error::invalid_type(
                other.unexpected(),
                &"unit variant",
            )),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self.value {
            Some(value) => seed.deserialize(value),
            None => Err(serde::de::Error::invalid_type(
                serde::de::Unexpected::UnitVariant,
                &"newtype variant",
            )),
        }
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.value {
            Some(seq @ Value::Seq(_)) => seq.deserialize_any(visitor),
            Some(other) => Err(serde::de::Error::invalid_type(
                other.unexpected(),
                &"tuple variant",
            )),
            None => Err(serde::de::Error::invalid_type(
                serde::de::Unexpected::UnitVariant,
                &"tuple variant",
            )),
        }
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.value {
            Some(map @ Value::Map(_)) => map.deserialize_any(visitor),
            Some(seq @ Value::Seq(_)) => seq.deserialize_any(visitor),
            Some(other) => Err(serde::de::Error::invalid_type(
                other.unexpected(),
                &"struct variant",
            )),
            None => Err(serde::de::Error::invalid_type(
                serde::de::Unexpected::UnitVariant,
                &"struct variant",
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    use std::{collections::BTreeMap, fmt::Debug};

    fn assert_same<'de, T>(s: &'de str)
    where
        T: Debug + Deserialize<'de> + PartialEq,
    {
        use crate::de::from_str;

        let direct: T = from_str(s).unwrap();
        let value: Value = from_str(s).unwrap();
        let value = T::deserialize(value).unwrap();

        assert_eq!(direct, value, "Deserialization for {:?} is not the same", s);
    }

    #[test]
    fn boolean() {
        assert_same::<bool>("true");
        assert_same::<bool>("false");
    }

    #[test]
    fn float() {
        assert_same::<f64>("0.123");
        assert_same::<f64>("-4.19");
    }

    #[test]
    fn int() {
        assert_same::<u32>("626");
        assert_same::<i32>("-50");
    }

    #[test]
    fn char() {
        assert_same::<char>("'4'");
        assert_same::<char>("'c'");
    }

    #[test]
    fn map() {
        assert_same::<BTreeMap<char, String>>(
            "{
'a': \"Hello\",
'b': \"Bye\",
        }",
        );
    }

    #[test]
    fn option() {
        assert_same::<Option<char>>("Some('a')");
        assert_same::<Option<char>>("None");
    }

    #[test]
    fn seq() {
        assert_same::<Vec<f64>>("[1.0, 2.0, 3.0, 4.0]");
    }

    #[test]
    fn unit() {
        assert_same::<()>("()");
    }
}
