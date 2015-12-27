#! /usr/bin/env perl
# Use cdo to extract subset of polish data to Poland dir
# Jane Coates 23/12/2015

use strict;
use diagnostics;

my @files = glob "wrf*_jane";
foreach my $file (@files) {
    my $new_file = $file . "_germany.nc";
    system "cdo sellonlatbox,9,14,50,53 $file Germany/$new_file";
}
