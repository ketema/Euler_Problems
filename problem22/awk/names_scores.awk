#!/usr/bin/awk -f
# Project Euler Problem 22: Names Scores
# Calculate the total of all name scores in a file
#
# Usage: awk -f names_scores.awk names.txt
# Or: ./names_scores.awk names.txt

BEGIN {
    # Configuration
    FS = ","  # Field separator is comma
    total_score = 0
    name_count = 0

    if (ARGC < 2 && !testing_mode) {
        print "Usage: awk -f names_scores.awk <names_file>"
        print "Example: awk -f names_scores.awk ../names.txt"
        exit 1
    }
}

# Main processing: read and store names
{
    # Each line contains comma-separated quoted names
    for (i = 1; i <= NF; i++) {
        # Remove quotes from the name
        name = $i
        gsub(/"/, "", name)  # Remove all quotes
        gsub(/^[ \t]+|[ \t]+$/, "", name)  # Trim whitespace

        if (name != "") {
            names[++name_count] = name
        }
    }
}

END {
    # Sort the names alphabetically
    sorted_count = sort_names(names, sorted_names)

    # Calculate total score
    total_score = calculate_total_score(sorted_names, sorted_count)

    # Output results
    if (!testing_mode) {
        print "Project Euler Problem 22: Names Scores"
        print "======================================="
        print ""
        print "Total names: " sorted_count
        print "Total score: " total_score
        print ""

        # Show first few examples
        print "First 5 names and scores:"
        for (i = 1; i <= 5 && i <= sorted_count; i++) {
            name = sorted_names[i]
            alpha_val = alphabetical_value(name)
            score = calculate_name_score(name, i)
            printf "  %d. %s (value=%d, score=%d)\n", i, name, alpha_val, score
        }
    }
}

# Calculate alphabetical value of a name
# A=1, B=2, ..., Z=26
function alphabetical_value(name,    i, char, value, sum) {
    sum = 0
    for (i = 1; i <= length(name); i++) {
        char = substr(name, i, 1)
        # Convert char to uppercase and get ASCII value
        # A=65 in ASCII, so subtract 64 to get A=1
        value = (index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", toupper(char)))
        if (value > 0) {
            sum += value
        }
    }
    return sum
}

# Calculate score for a single name at given position
function calculate_name_score(name, position) {
    return alphabetical_value(name) * position
}

# Sort names alphabetically using insertion sort
# Input: input_array - unsorted array of names
# Output: output_array - sorted array of names
# Returns: count of sorted names
function sort_names(input_array, output_array,    i, j, temp, count) {
    # Copy input to output
    count = 0
    for (i in input_array) {
        output_array[++count] = input_array[i]
    }

    # Insertion sort
    for (i = 2; i <= count; i++) {
        temp = output_array[i]
        j = i - 1
        while (j > 0 && output_array[j] > temp) {
            output_array[j + 1] = output_array[j]
            j--
        }
        output_array[j + 1] = temp
    }

    return count
}

# Calculate total score for all names
function calculate_total_score(names_array, count,    i, total) {
    total = 0
    for (i = 1; i <= count; i++) {
        total += calculate_name_score(names_array[i], i)
    }
    return total
}
