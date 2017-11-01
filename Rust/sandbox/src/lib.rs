#[cfg(test)]
mod tests {
    #[test]
    fn updating_a_vector_element() {
        struct S {
            x: i32,
            y: i32
        }
        let mut v: Vec<S> = vec![S{x:4807,y:1000}];
        v[0].x = 256;
        assert_eq!(v[0].x, 256);
    }

    #[test]
    fn test_reading_an_int() {
        use std::str::FromStr;
        let p = i32::from_str("4807\n".trim());
        let n = match p {
                Ok(n) => n,
                Err(m) => { println!("{}",m); -1 }
        };
        assert_eq!(4807, n);
    }
}
