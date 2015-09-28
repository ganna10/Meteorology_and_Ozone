#! /usr/bin/env perl
# extract data from individual mechanism files and output to csv file. Date (ddmmyyyy) and variable to be related to NOx emissions and Temperature are arguments
# Version 0: Jane Coates 27/8/2015

use strict;
use diagnostics;
use Cwd qw( cwd );

die "Need Variables and date" if (@ARGV == 0);
my $date = $ARGV[0];
shift @ARGV;
my @variables = qw( NOx.Emissions Temperature );
push @variables, @ARGV;

my $base = cwd();
opendir DIR, $base or die $!;
my @files = grep { $_ =~ /_check_Temperature_${date}\.txt$/ } readdir DIR;
close DIR;

my $out_file = "out_Temperature_NOx_${date}.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism";
foreach my $item (@variables) {
    print $out ",$item";
}
print $out "\n";

foreach my $checkfile (@files) { 
    (my $mechanism = $checkfile) =~ s/^(.*?)_check_(.*?)$/$1/;
    my ($nox_emission, $temperature, $variable) = extract_data($checkfile, \@variables);

    for (0..$#$nox_emission-1) {
        print $out "$mechanism,$nox_emission->[$_],$temperature->[$_],$variable->[$_]\n";
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

    my (@nox_emissions, @temperature, @variable);
    foreach my $variable (@$variables) { 
        if ($variable eq "NOx.Emissions") {
            foreach my $line (@lines) { 
                if ($line =~ /NOx emissions/) {
                    push @nox_emissions, get_value($line);
                }
            }
        } elsif ($variable eq "Temperature") {
            foreach my $line (@lines) { 
                if ($line =~ /NO_scaling/) {
                    (my $temperature = $line) =~ s/^(.*?)_T//;
                    $temperature =~ s/VOC_(.*?)$//;
                    $temperature =~ s/_//g;
                    push @temperature, $temperature;
                }
            }
        } else {
            foreach my $line (@lines) {
                if ($line =~ / $variable /) {
                    push @variable, get_value($line);
                }
            }
        }
    }
    return \@nox_emissions, \@temperature, \@variable;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
