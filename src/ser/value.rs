use serde::ser::{self, Serialize, Serializer};

use crate::error::{Error, Result};
use crate::value::{Number, Value};

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Bool(b) => serializer.serialize_bool(*b),
            Value::Char(c) => serializer.serialize_char(*c),
            Value::Map(m) => Serialize::serialize(m, serializer),
            Value::Number(Number::F32(f)) => serializer.serialize_f32(f.get()),
            Value::Number(Number::F64(f)) => serializer.serialize_f64(f.get()),
            Value::Number(Number::I8(i)) => serializer.serialize_i8(*i),
            Value::Number(Number::I16(i)) => serializer.serialize_i16(*i),
            Value::Number(Number::I32(i)) => serializer.serialize_i32(*i),
            Value::Number(Number::I64(i)) => serializer.serialize_i64(*i),
            #[cfg(feature = "integer128")]
            Value::Number(Number::I128(i)) => serializer.serialize_i128(*i),
            Value::Number(Number::U8(u)) => serializer.serialize_u8(*u),
            Value::Number(Number::U16(u)) => serializer.serialize_u16(*u),
            Value::Number(Number::U32(u)) => serializer.serialize_u32(*u),
            Value::Number(Number::U64(u)) => serializer.serialize_u64(*u),
            #[cfg(feature = "integer128")]
            Value::Number(Number::U128(u)) => serializer.serialize_u128(*u),
            Value::Option(Some(ref o)) => serializer.serialize_some(o.as_ref()),
            Value::Option(None) => serializer.serialize_none(),
            Value::String(s) => serializer.serialize_str(s),
            Value::Seq(s) => Serialize::serialize(s, serializer),
            Value::Unit => serializer.serialize_unit(),
        }
    }
}

struct ValueSerializer;

impl ser::Serializer for ValueSerializer {
    type Error = Error;
    type Ok = Value;

    type SerializeMap = ser::Impossible<Value, Error>; //Compound;
    type SerializeSeq = ser::Impossible<Value, Error>; //Compound;
    type SerializeStruct = ser::Impossible<Value, Error>; //Compound;
    type SerializeStructVariant = ser::Impossible<Value, Error>; //Compound;
    type SerializeTuple = ser::Impossible<Value, Error>; //Compound;
    type SerializeTupleStruct = ser::Impossible<Value, Error>; //Compound;
    type SerializeTupleVariant = ser::Impossible<Value, Error>; //Compound;

    fn serialize_bool(self, v: bool) -> Result<Value> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_i16(self, v: i16) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_i32(self, v: i32) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_i64(self, v: i64) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    #[cfg(feature = "integer128")]
    fn serialize_i128(self, v: i128) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_u8(self, v: u8) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_u16(self, v: u16) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_u32(self, v: u32) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_u64(self, v: u64) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    #[cfg(feature = "integer128")]
    fn serialize_u128(self, v: u128) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_f32(self, v: f32) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_f64(self, v: f64) -> Result<Value> {
        Ok(Value::Number(Number::new(v)))
    }

    fn serialize_char(self, v: char) -> Result<Value> {
        Ok(Value::Char(v))
    }

    fn serialize_str(self, v: &str) -> Result<Value> {
        Ok(Value::String(String::from(v)))
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Value> {
        todo!()
    }

    fn serialize_none(self) -> Result<Value> {
        Ok(Value::Option(None))
    }

    fn serialize_some<T>(self, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        let inner = value.serialize(ValueSerializer)?;

        Ok(Value::Option(Some(Box::new(inner))))
    }

    fn serialize_unit(self) -> Result<Value> {
        Ok(Value::Unit)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        todo!()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Value> {
        todo!()
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, _value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        todo!()
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        todo!()
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        todo!()
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        todo!()
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        todo!()
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        todo!()
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        todo!()
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        todo!()
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        todo!()
    }
}

/*#[doc(hidden)]
pub struct Compound {

}

impl ser::SerializeSeq for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let State::First = self.state {
            self.state = State::Rest;
        } else {
            self.ser.output.write_all(b",")?;
            if let Some((ref config, ref mut pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit && !config.compact_arrays {
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                } else {
                    self.ser.output.write_all(config.separator.as_bytes())?;
                }
            }
        }

        if !self.ser.compact_arrays() {
            self.ser.indent()?;
        }

        if let Some((ref mut config, ref mut pretty)) = self.ser.pretty {
            if pretty.indent <= config.depth_limit && config.enumerate_arrays {
                let index = pretty.sequence_index.last_mut().unwrap();
                write!(self.ser.output, "/*[{}]*/
 ", index)?;
                *index += 1;
            }
        }

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<()> {
        if let State::Rest = self.state {
            if let Some((ref config, ref mut pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit && !config.compact_arrays {
                    self.ser.output.write_all(b",")?;
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                }
            }
        }

        if !self.ser.compact_arrays() {
            self.ser.end_indent()?;
        }

        if let Some((_, ref mut pretty)) = self.ser.pretty {
            pretty.sequence_index.pop();
        }

        // seq always disables `self.newtype_variant`
        self.ser.output.write_all(b"]")?;
        Ok(())
    }
}

impl ser::SerializeTuple for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let State::First = self.state {
            self.state = State::Rest;
        } else {
            self.ser.output.write_all(b",")?;
            if let Some((ref config, ref pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit && self.ser.separate_tuple_members() {
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                } else {
                    self.ser.output.write_all(config.separator.as_bytes())?;
                }
            }
        }

        if self.ser.separate_tuple_members() {
            self.ser.indent()?;
        }

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<()> {
        if let State::Rest = self.state {
            if let Some((ref config, ref pretty)) = self.ser.pretty {
                if self.ser.separate_tuple_members() && pretty.indent <= config.depth_limit {
                    self.ser.output.write_all(b",")?;
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                }
            }
        }
        if self.ser.separate_tuple_members() {
            self.ser.end_indent()?;
        }

        if !self.newtype_variant {
            self.ser.output.write_all(b")")?;
        }

        Ok(())
    }
}

impl ser::SerializeTupleStruct for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<()> {
        ser::SerializeTuple::end(self)
    }
}

impl ser::SerializeTupleVariant for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<()> {
        ser::SerializeTuple::end(self)
    }
}

impl ser::SerializeMap for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let State::First = self.state {
            self.state = State::Rest;
        } else {
            self.ser.output.write_all(b",")?;

            if let Some((ref config, ref pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit {
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                } else {
                    self.ser.output.write_all(config.separator.as_bytes())?;
                }
            }
        }
        self.ser.indent()?;
        key.serialize(&mut *self.ser)
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.ser.output.write_all(b":")?;

        if let Some((ref config, _)) = self.ser.pretty {
            self.ser.output.write_all(config.separator.as_bytes())?;
        }

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<()> {
        if let State::Rest = self.state {
            if let Some((ref config, ref pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit {
                    self.ser.output.write_all(b",")?;
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                }
            }
        }
        self.ser.end_indent()?;
        // map always disables `self.newtype_variant`
        self.ser.output.write_all(b"}")?;
        Ok(())
    }
}

impl ser::SerializeStruct for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let State::First = self.state {
            self.state = State::Rest;
        } else {
            self.ser.output.write_all(b",")?;

            if let Some((ref config, ref pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit {
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                } else {
                    self.ser.output.write_all(config.separator.as_bytes())?;
                }
            }
        }
        self.ser.indent()?;
        self.ser.write_identifier(key)?;
        self.ser.output.write_all(b":")?;

        if let Some((ref config, _)) = self.ser.pretty {
            self.ser.output.write_all(config.separator.as_bytes())?;
        }

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<()> {
        if let State::Rest = self.state {
            if let Some((ref config, ref pretty)) = self.ser.pretty {
                if pretty.indent <= config.depth_limit {
                    self.ser.output.write_all(b",")?;
                    self.ser.output.write_all(config.new_line.as_bytes())?;
                }
            }
        }
        self.ser.end_indent()?;
        if !self.newtype_variant {
            self.ser.output.write_all(b")")?;
        }
        Ok(())
    }
}

impl ser::SerializeStructVariant for Compound {
    type Error = Error;
    type Ok = Value;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<()> {
        ser::SerializeStruct::end(self)
    }
}*/
