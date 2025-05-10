#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib $FindBin::Bin;
require 'matrix_product.pl';
use Term::ANSIColor;
use Scalar::Util qw(looks_like_number);

sub read_matrix {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Failed to read matrix from $filename: $!\n";
    my @matrix;
    while (my $line = <$fh>) {
        chomp $line;
        my @row = grep { looks_like_number($_) } split /\s+/, $line;
        push @matrix, \@row if @row;
    }
    close $fh;
    return \@matrix;
}

sub print_matrix_with_highlight {
    my ($matrix, $coords) = @_;
    my %coord_set = map { join(',', @$_) => 1 } @$coords;
    for my $i (0..$#$matrix) {
        for my $j (0..$#{$matrix->[$i]}) {
            my $val = $matrix->[$i][$j];
            if ($coord_set{"$i,$j"}) {
                print color('red'), sprintf("%02d ", $val), color('reset');
            } else {
                print sprintf("%02d ", $val);
            }
        }
        print "\n";
    }
}

# Main
my $filename = $ARGV[0] // 'matrix.txt';
my $matrix = read_matrix($filename);
my ($result, $coords) = greatest_product($matrix, 4);
print_matrix_with_highlight($matrix, $coords);
print "Greatest product of four adjacent numbers: $result\n";
