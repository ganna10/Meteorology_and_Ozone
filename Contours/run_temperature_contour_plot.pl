#! /usr/bin/env perl
# Run temperature_contour_plot.R for different species. Date for check file is argument
# Version 0: Jane Coates 29/9/2015

use strict;
use diagnostics;

die "Need date ddmmyyy" if (@ARGV == 0);
my $date = $ARGV[0];

my @species = qw( O3 OH HO2 HCHO HNO3 H2O2 );

foreach my $spc (@species) {
    system "perl get_temperature_data_to_csv.pl $date $spc";
    system "Rscript temperature_contour_plot.R $date $spc";
}
