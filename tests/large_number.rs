use ron::value::{Number, Value};

#[test]
fn test_large_number() {
    let test_var = Value::Number(Number::new(10000000000000000000000.0f64));
    let test_ser = ron::ser::to_string_pretty(
        &test_var,
        ron::ser::PrettyConfig::new().decimal_floats(true),
    )
    .unwrap();
    let test_deser = ron::de::from_str::<Value>(&test_ser);

    assert_eq!(
        test_deser.unwrap(),
        Value::Number(Number::new(10000000000000000000000.0))
    );
}

#[test]
fn test_large_integer_to_float() {
    let test_var = std::i64::MAX as u64 + 1;
    let test_ser = ron::ser::to_string(&test_var).unwrap();
    assert_eq!(test_ser, test_var.to_string());
    let test_deser = ron::de::from_str::<Value>(&test_ser);

    assert_eq!(test_deser.unwrap(), Value::Number(Number::U64(test_var)),);
}
