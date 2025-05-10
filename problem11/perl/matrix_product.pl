use strict;
use warnings;

# Returns ($max_product, $coords)
sub greatest_product {
    my ($matrix, $adj) = @_;
    my $rows = scalar(@$matrix);
    my $cols = scalar(@{$matrix->[0]});
    my $max = 0;
    my $best_coords = [];
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            # right
            if ($j + $adj <= $cols) {
                my $prod = 1;
                my @coords;
                for my $k (0..$adj-1) {
                    $prod *= $matrix->[$i][$j+$k];
                    push @coords, [$i, $j+$k];
                }
                if ($prod > $max) {
                    $max = $prod;
                    $best_coords = [@coords];
                }
            }
            # down
            if ($i + $adj <= $rows) {
                my $prod = 1;
                my @coords;
                for my $k (0..$adj-1) {
                    $prod *= $matrix->[$i+$k][$j];
                    push @coords, [$i+$k, $j];
                }
                if ($prod > $max) {
                    $max = $prod;
                    $best_coords = [@coords];
                }
            }
            # diag down-right
            if ($i + $adj <= $rows && $j + $adj <= $cols) {
                my $prod = 1;
                my @coords;
                for my $k (0..$adj-1) {
                    $prod *= $matrix->[$i+$k][$j+$k];
                    push @coords, [$i+$k, $j+$k];
                }
                if ($prod > $max) {
                    $max = $prod;
                    $best_coords = [@coords];
                }
            }
            # diag down-left
            if ($i + $adj <= $rows && $j - $adj + 1 >= 0) {
                my $prod = 1;
                my @coords;
                for my $k (0..$adj-1) {
                    $prod *= $matrix->[$i+$k][$j-$k];
                    push @coords, [$i+$k, $j-$k];
                }
                if ($prod > $max) {
                    $max = $prod;
                    $best_coords = [@coords];
                }
            }
        }
    }
    return ($max, $best_coords);
}

1;
