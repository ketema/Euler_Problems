// Tests for input validation and user input handling for problem 12

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_input(input: &str) -> Result<u32, String> {
        input.trim().parse::<u32>().map_err(|_| "Please enter a valid numeric value.".to_string())
    }

    #[test]
    fn test_valid_numeric_input() {
        assert_eq!(parse_input("28"), Ok(28));
        assert_eq!(parse_input("  42  "), Ok(42));
    }

    #[test]
    fn test_invalid_input_non_numeric() {
        assert_eq!(parse_input("abc"), Err("Please enter a valid numeric value.".to_string()));
        assert_eq!(parse_input("12abc"), Err("Please enter a valid numeric value.".to_string()));
    }

    #[test]
    fn test_invalid_input_empty() {
        assert_eq!(parse_input(""), Err("Please enter a valid numeric value.".to_string()));
    }
}
