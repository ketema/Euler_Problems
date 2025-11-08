"""Problem 19: Counting Sundays (1 Jan 1901 - 31 Dec 2000)"""
from datetime import date, timedelta

def count_sundays_on_first():
    """Count Sundays that fell on first of month in 20th century"""
    count = 0
    current = date(1901, 1, 1)
    end = date(2000, 12, 31)

    while current <= end:
        if current.day == 1 and current.weekday() == 6:  # Sunday = 6
            count += 1
        # Move to first of next month
        if current.month == 12:
            current = date(current.year + 1, 1, 1)
        else:
            current = date(current.year, current.month + 1, 1)

    return count

if __name__ == "__main__":
    result = count_sundays_on_first()
    print(f"Problem 19 Answer: {result}")
    assert result == 171  # Known answer
