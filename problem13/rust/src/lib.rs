use num_bigint::BigInt;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum Problem13Error {
    InvalidInput(String),
    ParseError(String),
    InsufficientDigits(String),
}

impl std::fmt::Display for Problem13Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Problem13Error::InvalidInput(msg) => write!(f, "Invalid input: {}", msg),
            Problem13Error::ParseError(msg) => write!(f, "Parse error: {}", msg),
            Problem13Error::InsufficientDigits(msg) => write!(f, "Insufficient digits: {}", msg),
        }
    }
}

impl std::error::Error for Problem13Error {}

pub fn parse_input_numbers(input: &str) -> Result<Vec<BigInt>, Problem13Error> {
    let lines: Vec<&str> = input.trim().split('\n').collect();
    
    if lines.len() != 100 {
        return Err(Problem13Error::InvalidInput(
            format!("Expected 100 numbers, got {}", lines.len())
        ));
    }
    
    let mut numbers = Vec::new();
    
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        
        if trimmed.len() != 50 {
            return Err(Problem13Error::InvalidInput(
                format!("Line {} has {} digits, expected 50", i + 1, trimmed.len())
            ));
        }
        
        if !trimmed.chars().all(|c| c.is_ascii_digit()) {
            return Err(Problem13Error::InvalidInput(
                format!("Line {} contains non-digit characters", i + 1)
            ));
        }
        
        match BigInt::from_str(trimmed) {
            Ok(num) => numbers.push(num),
            Err(e) => return Err(Problem13Error::ParseError(
                format!("Failed to parse line {}: {}", i + 1, e)
            )),
        }
    }
    
    Ok(numbers)
}

pub fn sum_large_numbers(numbers: Vec<BigInt>) -> BigInt {
    numbers.into_iter().sum()
}

pub fn extract_first_n_digits(number: &BigInt, n: usize) -> Result<String, Problem13Error> {
    let number_str = number.to_string();
    
    if number_str.len() >= n {
        Ok(number_str.chars().take(n).collect())
    } else {
        // Pad with zeros on the right to reach n digits
        let mut result = number_str;
        while result.len() < n {
            result.push('0');
        }
        Ok(result)
    }
}

pub fn solve_problem13(input_text: &str) -> Result<String, Problem13Error> {
    let numbers = parse_input_numbers(input_text)?;
    let sum = sum_large_numbers(numbers);
    extract_first_n_digits(&sum, 10)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid_input() {
        let input = "12345678901234567890123456789012345678901234567890\n98765432109876543210987654321098765432109876543210";
        let result = parse_input_numbers(input);
        assert!(result.is_err()); // Should fail because we need 100 numbers, not 2
    }

    #[test]
    fn test_sum_large_numbers() {
        let numbers = vec![
            BigInt::from_str("123").unwrap(),
            BigInt::from_str("456").unwrap(),
            BigInt::from_str("789").unwrap(),
        ];
        let sum = sum_large_numbers(numbers);
        assert_eq!(sum, BigInt::from_str("1368").unwrap());
    }

    #[test]
    fn test_extract_first_n_digits() {
        let number = BigInt::from_str("1234567890").unwrap();
        let result = extract_first_n_digits(&number, 5).unwrap();
        assert_eq!(result, "12345");
    }

    #[test]
    fn test_extract_first_n_digits_padding() {
        let number = BigInt::from_str("123").unwrap();
        let result = extract_first_n_digits(&number, 5).unwrap();
        assert_eq!(result, "12300");
    }
}
