use strict;
use warnings;
use Test::More;
use lib '.';
require 'matrix_product.pl';

my $matrix = [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
    [13, 14, 15, 16],
];
my ($result, $coords) = greatest_product($matrix, 4);
my $expected = 13 * 14 * 15 * 16;
my $expected_coords = [ [3,0],[3,1],[3,2],[3,3] ];

is($result, $expected, 'Correct greatest product for 4x4');
# Deep compare coordinates
is_deeply($coords, $expected_coords, 'Correct coordinates for greatest product');

done_testing();
