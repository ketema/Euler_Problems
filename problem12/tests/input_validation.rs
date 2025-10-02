// Tests for input validation and user input handling for problem 12

#[cfg(test)]
mod tests {
    use problem12::{parse_input, Problem12Error};

    #[test]
    fn test_valid_numeric_input() {
        assert_eq!(parse_input("28"), Ok(28));
        assert_eq!(parse_input("  42  "), Ok(42));
    }

    #[test]
    fn test_invalid_input_non_numeric() {
        assert_eq!(parse_input("abc"), Err(Problem12Error::InvalidInput("abc".to_string())));
        assert_eq!(parse_input("12abc"), Err(Problem12Error::InvalidInput("12abc".to_string())));
    }

    #[test]
    fn test_invalid_input_empty() {
        assert_eq!(parse_input(""), Err(Problem12Error::InvalidInput("".to_string())));
    }
}
