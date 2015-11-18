#! /usr/bin/env perl
# Allocate each reaction in Ox production budget to category: HO2, CH3O2, CH3CO3, RO2. Mechanism is ARGV
# Version 0: Jane Coates 18/11/2015

use strict;
use diagnostics;

die "Specify mechanism!" if (@ARGV == 0);
my $mechanism = $ARGV[0];

my $in_file = $mechanism . "_O3_budget_10112015.txt";
open my $in, '<:encoding(utf-8)', $in_file;
my @lines = <$in>;
close $in_file;

my @out_data;
my $out_file = $mechanism . "_O3_budget_18112015.csv";
