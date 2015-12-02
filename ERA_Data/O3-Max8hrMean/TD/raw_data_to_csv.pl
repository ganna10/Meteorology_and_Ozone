#! /usr/bin/env perl
# extract data from individual mechanism files and output to csv file. 
# Version 0: Jane Coates 30/11/2015

use strict;
use diagnostics;
use Cwd qw( cwd );

my @variables = qw( NOx.Emissions NOx Temperature O3 H2O2 OH HO2 HOx HCHO RO2NO2 RONO2 HNO3 PAN Ketones Aldehydes Max.O3.8hr.av );
my $base = cwd();
opendir DIR, $base or die $!;
my @files = grep { $_ =~ /_check_Temperature/ } readdir DIR;
close DIR;

my $out_file = "out_Temperature_NOx.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism";
foreach my $item (@variables) {
    print $out ",$item";
}
print $out "\n";

foreach my $checkfile (@files) { 
    (my $mechanism = $checkfile) =~ s/^(.*?)_check_(.*?)$/$1/;
    my ($nox_emission, $NOx, $temperature, $O3, $H2O2, $OH, $HO2, $HOx, $HCHO, $RO2NO2, $RONO2, $HNO3, $PAN, $Ketones, $Aldehydes, $max_O3_8h_av) = extract_data($mechanism, $checkfile, \@variables);

    for (0..$#$nox_emission) {
        $Ketones->[$_] = 0 if ($mechanism eq "CB05");
        print $out "$mechanism,$nox_emission->[$_],$NOx->[$_],$temperature->[$_],$O3->[$_],$H2O2->[$_],$OH->[$_],$HO2->[$_],$HOx->[$_],$HCHO->[$_],$RO2NO2->[$_],$RONO2->[$_],$HNO3->[$_],$PAN->[$_],$Ketones->[$_],$Aldehydes->[$_],$max_O3_8h_av->[$_]\n";
    }
}

close $out;

sub extract_data {
    my ($mechanism, $file, $variables) = @_;

    print "Extracting data from $file\n";
    open my $in, '<:encoding(utf-8)', $file or die $!;
    local $/ = undef; #slurp mode
    my $all_lines = <$in>;
    close $in; 
    my @lines = split /\n/, $all_lines;

    my (@nox_emissions, @NOx, @temperature, @O3, @H2O2, @OH, @HO2, @HOx, @HCHO, @RONO2, @RO2NO2, @HNO3, @PAN, @Ketones, @Aldehydes, @max_O3_8h_av);
    foreach my $variable (@$variables) { 
        if ($variable eq "NOx.Emissions") {
            foreach my $line (@lines) { 
                if ($line =~ /NOx emissions/) {
                    push @nox_emissions, get_value($line);
                }
            }
        } elsif ($variable eq "Temperature") {
            foreach my $line (@lines) { 
                if ($line =~ /^T_/) {
                    (my $temperature = $line) =~ s/^T_(.*?)_NOSF_(.*?)$/$1/;
                    push @temperature, $temperature;
                }
            }
        } elsif ($variable =~ /8hr/) {
            foreach my $line (@lines) {
                if ($line =~ /Maximum O3 8-Hr Average/) {
                    push @max_O3_8h_av, get_value($line) * 1e9;
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
                    } elsif ($variable eq "Ketones") {
                        push @Ketones, get_value($line) * 1e9;
                    } elsif ($variable eq "Aldehydes") {
                        push @Aldehydes, get_value($line) * 1e9;
                    } elsif ($variable eq "O3") {
                        push @O3, get_value($line) * 1e9;
                    } else {
                        print "Missing data structure for $variable\n" unless ($variable eq "NOx");
                    }
                }
            }
        }
    }
    return \@nox_emissions, \@NOx, \@temperature, \@O3, \@H2O2, \@OH, \@HO2, \@HOx, \@HCHO, \@RO2NO2, \@RONO2, \@HNO3, \@PAN, \@Ketones, \@Aldehydes, \@max_O3_8h_av;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
