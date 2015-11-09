#! /usr/bin/env perl
# Extract PAN = CH3CO3 + NO2 rate constant at each Temperature and NO condition. ARGV is data ddmmyyyy
# Version 0: Jane Coates 14/10/2015
#
use strict;
use diagnostics;

die "Date (ddmmyyyy) missing : $!" if (@ARGV == 0);
my @mechanisms = qw( MCMv3.2 CB05 CRIv2 MOZART-4 RADM2 );
my @data;
push @data, "Mechanism,Temperature,NOx.Emissions,Rate.Constant\n";

foreach my $mechanism (@mechanisms) {
    my $file = $mechanism . "_reaction_rate_constant_file_" . $ARGV[0] . ".txt";
    open my $in, '<:encoding(utf-8)', $file or die $!;
    my @lines = <$in>;
    close $in;

    my $thermal_decomposition;
    if ($mechanism eq "MCMv3.2" or $mechanism eq "CRIv2" or $mechanism eq "MOZART-4") {
        $thermal_decomposition = "PAN = CH3CO3 \+";
    } elsif ($mechanism eq "CB05") {
        $thermal_decomposition = "PAN = C2O3 \+";
    } elsif ($mechanism eq "RADM2") {
        $thermal_decomposition = "PAN = ACO3 \+";
    }

    my (@temperature, @NOx_emissions, @rate_constant);
    foreach my $line (@lines) {
        if ($line =~ /Temperature/) {
            my ($first, $temperature) = split / = /, $line;
            $temperature =~ s/ K//;
            $temperature =~ s/^\s+|\s+$//g;
            push @temperature, $temperature
        } elsif ($line =~ /NOx Emissions/) {
            my ($first, $NOx_emissions) = split / = /, $line;
            $NOx_emissions =~ s/ molecules cm-3//;
            $NOx_emissions =~ s/^\s+|\s+$//g;
            push @NOx_emissions, $NOx_emissions;
        } elsif ($line =~ /$thermal_decomposition/) {
            push @rate_constant, get_value($line);
        }
    }

    for (0..@temperature-1) {
        push @data, "$mechanism,$temperature[$_],$NOx_emissions[$_],$rate_constant[$_]\n";
    }
}

my $out_file = "PAN_thermal_decomposition_rate_constant_data.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out $_ foreach (@data);
close $out;

sub get_value {
    my ($line) = @_;
    chomp $line;
    my ($reactants, $products, $rate_constant) = split / = /, $line;
    return $rate_constant;
}
