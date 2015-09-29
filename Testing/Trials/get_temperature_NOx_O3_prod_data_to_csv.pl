#! /usr/bin/env perl
# extract data from individual mechanism files and output to csv file. Date (ddmmyyyy) to be related to NOx emissions and Temperature are arguments
# Version 0: Jane Coates 27/8/2015

use strict;
use diagnostics;
use Cwd qw( cwd );

die "Need date" if (@ARGV == 0);
my $date = $ARGV[0];
my @variables = qw( NOx.Emissions Temperature O3.Production);

my $base = cwd();
opendir DIR, $base or die $!;
my @files = grep { $_ =~ /_check_O3_prod_temperature_NOx_${date}\.txt$/ } readdir DIR;
close DIR;

my $out_file = "out_Temperature_NOx_O3_Production_${date}.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism";
foreach my $item (@variables) {
    print $out ",$item";
}
print $out "\n";

foreach my $checkfile (@files) { 
    (my $mechanism = $checkfile) =~ s/^(.*?)_check_(.*?)$/$1/;
    my ($nox_emission, $temperature, $o3_production) = extract_data($checkfile, \@variables);

    for (0..$#$nox_emission-1) {
        print $out "$mechanism,$nox_emission->[$_],$temperature->[$_],$o3_production->[$_]\n";
    }
}

close $out;

sub extract_data {
    my ($file, $variables) = @_;

    print "Extracting data from $file\n";
    open my $in, '<:encoding(utf-8)', $file or die $!;
    local $/ = undef; #slurp mode
    my $all_lines = <$in>;
    close $in; 
    my @lines = split /\n/, $all_lines;

    my (@nox_emissions, @temperature, @o3_production);
    foreach my $variable (@$variables) { 
        if ($variable eq "NOx.Emissions") {
            foreach my $line (@lines) { 
                if ($line =~ /NOx emissions/) {
                    push @nox_emissions, get_value($line);
                }
            }
        } elsif ($variable eq "Temperature") {
            foreach my $line (@lines) { 
                if ($line =~ /Temperature/) {
                    push @temperature, get_value($line);
                }
            }
        } else {
            foreach my $line (@lines) {
                if ($line =~ /O3 production rate/) {
                    push @o3_production, get_value($line);
                }
            }
        }
    }
    return \@nox_emissions, \@temperature, \@o3_production;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
