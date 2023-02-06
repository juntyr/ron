#[test]
fn test_serde_bytes() {
    #[derive(Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
    #[serde(rename = "b")]
    struct BytesVal {
        pub b: serde_bytes::ByteBuf,
    }

    #[derive(Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
    // #[serde(untagged)]
    enum Bad {
        Bytes(BytesVal),
    }

    let s = ron::to_string(&serde_bytes::Bytes::new(b"test")).unwrap();

    assert_eq!(s, r#""dGVzdA==""#);

    let v: Bad = ron::from_str(r#"Bytes((b: "dGVzdA=="))"#).unwrap();

    assert_eq!(format!("{:?}", v), "Bytes(BytesVal { b: [116, 101, 115, 116] })");

    let s = ron::to_string(&v).unwrap();

    assert_eq!(s, r#"Bytes((b:"dGVzdA=="))"#);
}

#[test]
fn test_bytes() {
    #[derive(Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
    #[serde(rename = "b")]
    struct BytesVal {
        pub b: bytes::Bytes,
    }

    #[derive(Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
    #[serde(untagged)]
    enum Bad {
        Bytes(BytesVal),
    }

    let v: Bad = ron::from_str(r#"(b: "dGVzdA==")"#).unwrap();

    assert_eq!(format!("{:?}", v), r#"Bytes(BytesVal { b: b"dGVzdA==" })"#);

    let s = ron::to_string(&v).unwrap();

    assert_eq!(s, r#"(b: "dGVzdA==")"#);
}
