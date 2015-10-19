#! /usr/bin/env perl
# extract data from individual mechanism files and output to csv file. Date (ddmmyyyy) is argument
# Version 0: Jane Coates 29/9/2015
# Version 1: Jane Coates 13/10/2015 outputting VOCR data

use strict;
use diagnostics;
use Cwd qw( cwd );

die "Need date" if (@ARGV == 0);
my $date = $ARGV[0];
my @variables = qw( NOx.Emissions NOx Scaling_Factor Temperature );

my $base = cwd();
opendir DIR, $base or die $!;
my @files = grep { $_ =~ /_check_Temperature_${date}\.txt$/ } readdir DIR;
close DIR;

my $out_file = "NOx_test.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism";
foreach my $item (@variables) {
    print $out ",$item";
}
print $out "\n";

foreach my $checkfile (@files) { 
    (my $mechanism = $checkfile) =~ s/^(.*?)_check_(.*?)$/$1/;
    my ($nox_emission, $NOx, $SF, $temperature) = extract_data($checkfile, \@variables);

    for (0..$#$nox_emission-1) {
        print $out "$mechanism,$nox_emission->[$_],$NOx->[$_],$SF->[$_],$temperature->[$_]\n";
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

    my (@nox_emissions, @NOx, @SF, @temperature);
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
        } elsif ($variable eq "Scaling_Factor") {
            foreach my $line (@lines) { 
                if ($line =~ /NO_scaling/) {
                    (my $SF = $line) =~ s/^(.*?)RH_(.*?)$/$1/;
                    $SF =~ s/NO_scaling_factor(.*?)$/$1/;
                    $SF =~ s/_//g;
                    $SF =~ s/^\s+|\s+$//g;
                    push @SF, $SF;
                }
            }
        } else {
            foreach my $line (@lines) {
                if ($line =~ / $variable /) {
                    if ($variable eq "NOx" and $line =~ /Max/) {
                        push @NOx, get_value($line);
                    }
                }
            }
        }
    }
    return \@nox_emissions, \@NOx, \@SF, \@temperature;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
