#! /usr/bin/env perl
# Extract PAN reaction rate constants for each reaction at each Temperature and NO condition. ARGV is data ddmmyyyy
# Version 0: Jane Coates 15/10/2015

use strict;
use diagnostics;

die "Date (ddmmyyyy) missing : $!" if (@ARGV == 0);
my @mechanisms = qw( CB05 CRIv2 MOZART-4 RADM2 );
my @data;
push @data, "Mechanism,Temperature,NOx.Mixing.Ratio,NOx.Emissions,Reaction,Rate.Constant\n";

foreach my $mechanism (@mechanisms) {
    my $file = $mechanism . "_reaction_rate_constant_file_" . $ARGV[0] . ".txt";
    open my $in, '<:encoding(utf-8)', $file or die $!;
    my @lines = <$in>;
    close $in;
    
    foreach my $line (@lines) {
        next if ($line =~ /NOx|NO_/);
        chomp $line;
        $line =~ s/^\s+|\s+$//g;
        if ($mechanism eq "CB05") {
            $line =~ s/C2O3/CH3CO3/g;
        } elsif ($mechanism eq "RADM2") {
            $line =~ s/ACO3/CH3CO3/g;
        }
        my ($temperature, $NOx_MR, $NOx_emissions, $reaction, $k) = split / : /, $line;
        push @data, "$mechanism,$temperature,$NOx_MR,$NOx_emissions,$reaction,$k\n";
    } 
}

my $out_file = "RO2NO2_budget_reaction_rate_constant_data.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out $_ foreach (@data);
close $out;
