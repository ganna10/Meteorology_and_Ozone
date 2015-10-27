#! /usr/bin/env perl
# extract data from individual mechanism files and output to csv file. Date (ddmmyyyy) is argument
# Version 0: Jane Coates 29/9/2015
# Version 1: Jane Coates 13/10/2015 outputting VOCR data
# Version 2: Jane Coates 27/10/2015 not including VOCR

use strict;
use diagnostics;
use Cwd qw( cwd );

die "Need date" if (@ARGV == 0);
my $date = $ARGV[0];
my @variables = qw( NOx.Emissions NOx Temperature O3 H2O2 OH HO2 HOx HCHO RO2NO2 RONO2 HNO3 PAN );

my $base = cwd();
opendir DIR, $base or die $!;
my @files = grep { $_ =~ /_check_Temperature_${date}\.txt$/ } readdir DIR;
#my @files = grep { $_ =~ /MCMv3\.2_check_Temperature_${date}\.txt$/ } readdir DIR;
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
    my ($nox_emission, $NOx, $temperature, $O3, $H2O2, $OH, $HO2, $HOx, $HCHO, $RO2NO2, $RONO2, $HNO3, $PAN) = extract_data($checkfile, \@variables);
    #my ($nox_emission, $NOx, $temperature, $O3, $VOCR, $H2O2, $OH, $HO2, $HOx, $HCHO, $RO2NO2, $RONO2, $HNO3, $PAN) = extract_data($checkfile, \@variables);

    for (0..$#$nox_emission) {
        print $out "$mechanism,$nox_emission->[$_],$NOx->[$_],$temperature->[$_],$O3->[$_],$H2O2->[$_],$OH->[$_],$HO2->[$_],$HOx->[$_],$HCHO->[$_],$RO2NO2->[$_],$RONO2->[$_],$HNO3->[$_],$PAN->[$_]\n";
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

    my (@nox_emissions, @NOx, @temperature, @O3, @H2O2, @OH, @HO2, @HOx, @HCHO, @RONO2, @RO2NO2, @HNO3, @PAN);
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
                    if ($variable eq "OH") { 
                        push @OH, get_value($line) * 1e12;
                    } elsif ($variable eq "HO2") {
                        push @HO2, get_value($line) * 1e12;
                    } elsif ($variable eq "HOx") {
                        push @HOx, get_value($line) * 1e12;
                    } elsif ($variable eq "HNO3") {
                        push @HNO3, get_value($line) * 1e9;
                    } elsif ($variable eq "H2O2") {
                        push @H2O2, get_value($line) * 1e9;
                    } elsif ($variable eq "HCHO") {
                        push @HCHO, get_value($line) * 1e9;
                    } elsif ($variable eq "NOx" and $line =~ /Max/) {
                        push @NOx, get_value($line) * 1e9;;
                    } elsif ($variable eq "RO2NO2") {
                        push @RO2NO2, get_value($line) * 1e9;
                    } elsif ($variable eq "RONO2") {
                        push @RONO2, get_value($line) * 1e9;
                    } elsif ($variable eq "PAN") {
                        push @PAN, get_value($line) * 1e9;
                    } elsif ($variable eq "O3") {
                        push @O3, get_value($line) * 1e9;
                    } elsif ($variable eq "VOCR") {
                        #push @VOCR, get_value($line);
                    } else {
                        print "Missing data structure for $variable\n" unless ($variable eq "NOx");
                    }
                }
            }
        }
    }
    return \@nox_emissions, \@NOx, \@temperature, \@O3, \@H2O2, \@OH, \@HO2, \@HOx, \@HCHO, \@RO2NO2, \@RONO2, \@HNO3, \@PAN;
    #my ($nox_emission, $NOx, $temperature, $O3, $H2O2, $OH, $HO2, $HOx, $HCHO, $RO2NO2, $RONO2, $HNO3, $PAN) = extract_data($checkfile, \@variables);
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
