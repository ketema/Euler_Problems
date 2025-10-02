#[cfg(test)]
mod tests {
    use problem12::{Problem12Error, parse_input};

    #[test]
    fn test_invalid_input_error_display() {
        let error = Problem12Error::InvalidInput("abc".to_string());
        assert_eq!(format!("{}", error), "Invalid input: abc");
    }

    #[test]
    fn test_overflow_error_display() {
        let error = Problem12Error::Overflow(u64::MAX);
        assert_eq!(format!("{}", error), format!("Overflow detected with value: {}", u64::MAX));
    }

    #[test]
    fn test_timeout_error_display() {
        let error = Problem12Error::Timeout(std::time::Duration::from_secs(5));
        assert_eq!(format!("{}", error), "Operation timed out after 5s");
    }

    #[test]
    fn test_parse_input_invalid_non_numeric() {
        let result = parse_input("abc");
        match result {
            Err(Problem12Error::InvalidInput(msg)) => assert_eq!(msg, "abc"),
            _ => panic!("Expected InvalidInput error"),
        }
    }

    #[test]
    fn test_parse_input_invalid_empty() {
        let result = parse_input("");
        match result {
            Err(Problem12Error::InvalidInput(msg)) => assert_eq!(msg, ""),
            _ => panic!("Expected InvalidInput error"),
        }
    }

    #[test]
    fn test_parse_input_valid_numeric() {
        let result = parse_input("42");
        assert_eq!(result, Ok(42));
    }

    #[test]
    fn test_parse_input_valid_with_whitespace() {
        let result = parse_input("  28  ");
        assert_eq!(result, Ok(28));
    }
}
