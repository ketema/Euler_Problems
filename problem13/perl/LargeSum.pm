package LargeSum;

use strict;
use warnings;
use Math::BigInt;
use Carp qw(croak);

=head1 NAME

LargeSum - Project Euler Problem 13: Large Sum

=head1 SYNOPSIS

    use LargeSum;
    
    my $solver = LargeSum->new();
    my $result = $solver->solve_problem13($input_text);
    print "First ten digits: $result\n";

=head1 DESCRIPTION

This module solves Project Euler Problem 13: finding the first ten digits
of the sum of one hundred 50-digit numbers.

=head1 METHODS

=head2 new()

Creates a new LargeSum instance.

=cut

sub new {
    my $class = shift;
    return bless {}, $class;
}

=head2 parse_input_numbers($input_text)

Parses input text into an array of Math::BigInt objects.

=cut

sub parse_input_numbers {
    my ($self, $input_text) = @_;
    
    my @lines = split /\n/, $input_text;
    @lines = grep { $_ =~ /\S/ } @lines;  # Remove empty lines
    
    if (@lines != 100) {
        croak "Expected 100 numbers, got " . scalar(@lines);
    }
    
    my @numbers;
    for my $i (0 .. $#lines) {
        my $line = $lines[$i];
        $line =~ s/^\s+|\s+$//g;  # Trim whitespace
        
        if (length($line) != 50) {
            croak "Line " . ($i + 1) . " has " . length($line) . " digits, expected 50";
        }
        
        if ($line !~ /^\d+$/) {
            croak "Line " . ($i + 1) . " contains non-digit characters";
        }
        
        push @numbers, Math::BigInt->new($line);
    }
    
    return @numbers;
}

=head2 sum_large_numbers(@numbers)

Sums an array of Math::BigInt objects.

=cut

sub sum_large_numbers {
    my ($self, @numbers) = @_;
    
    my $sum = Math::BigInt->new(0);
    for my $number (@numbers) {
        $sum->badd($number);  # In-place addition
    }
    
    return $sum;
}

=head2 extract_first_n_digits($number, $n)

Extracts the first n digits from a Math::BigInt number.

=cut

sub extract_first_n_digits {
    my ($self, $number, $n) = @_;
    
    my $number_str = $number->bstr();
    
    if (length($number_str) >= $n) {
        return substr($number_str, 0, $n);
    } else {
        # Pad with zeros on the right to reach n digits
        return $number_str . ('0' x ($n - length($number_str)));
    }
}

=head2 solve_problem13($input_text)

Solves Project Euler Problem 13.

=cut

sub solve_problem13 {
    my ($self, $input_text) = @_;
    
    my @numbers = $self->parse_input_numbers($input_text);
    my $sum = $self->sum_large_numbers(@numbers);
    return $self->extract_first_n_digits($sum, 10);
}

1;

__END__

=head1 AUTHOR

Ketema Harris <ketema@ketema.net>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2025 by Ketema Harris.

=cut
