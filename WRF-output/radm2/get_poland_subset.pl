#! /usr/bin/env perl
# Use cdo to extract subset of polish data to Poland dir
# Jane Coates 23/12/2015

use strict;
use diagnostics;

my @files = glob "wrf*_jane";
foreach my $file (@files) {
    my $new_file = $file . "_poland.nc";
    system "cdo sellonlatbox,15,20,50,52 $file Poland/$new_file";
}
