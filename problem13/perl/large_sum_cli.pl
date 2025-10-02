#!/usr/bin/env perl

use strict;
use warnings;
use FindBin qw($Bin);
use lib $Bin;
use LargeSum;

# Read the numbers file
my $numbers_file = "$Bin/../numbers.txt";

open my $fh, '<', $numbers_file or die "Cannot open $numbers_file: $!";
my $input_text = do { local $/; <$fh> };
close $fh;

# Solve the problem
my $solver = LargeSum->new();
eval {
    my $result = $solver->solve_problem13($input_text);
    print "First ten digits of the sum: $result\n";
};

if ($@) {
    print STDERR "Error solving problem: $@\n";
    exit 1;
}
