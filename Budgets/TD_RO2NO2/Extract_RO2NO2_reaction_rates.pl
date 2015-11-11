#! /usr/bin/env perl
# Extract PAN = CH3CO3 + NO2 rate constant at each Temperature and NO condition. ARGV is data ddmmyyyy
# Version 0: Jane Coates 11/10/2015
#
use strict;
use diagnostics;

die "Date (ddmmyyyy) and mechanism missing : $!" if (@ARGV == 0);
my $date = $ARGV[0];
my @mechanisms = qw( CB05 RADM2 MOZART-4 CRIv2 MCMv3.2 );
my @data;
push @data, "Mechanism,Temperature,NOx.Emissions,Reaction,Rate.Constant\n";

foreach my $mechanism (@mechanisms) {
    my $file = $mechanism . "_reaction_rate_constant_file_" . $date . ".txt";
    open my $in, '<:encoding(utf-8)', $file or die $!;
    my @lines = <$in>;
    close $in;

    my (@temperature, @NOx_emissions, @reaction, @rate_constant);
    foreach my $line (@lines) {
        next if ($line =~ /^T_|Temperature/);
        my ($temperature, $NOx_mr, $NOx_emissions, $reaction, $rate_constant) = split / : /, $line;
        $temperature =~ s/^\s+|\s+$//g;
        $rate_constant =~ s/^\s#|\s+$//g;
        push @data, "$mechanism,$temperature,$NOx_emissions,$reaction,$rate_constant\n";
    }
}

my $out_file = "RO2NO2_rate_constant_data.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out $_ foreach (@data);
close $out;

sub get_value {
    my ($line) = @_;
    chomp $line;
    my ($reactants, $products, $rate_constant) = split / = /, $line;
    return $rate_constant;
}
