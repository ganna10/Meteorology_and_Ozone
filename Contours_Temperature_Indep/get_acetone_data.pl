#! /usr/bin/env perl
# extract data from individual mechanism files and output to csv file. Date (ddmmyyyy) is argument
# Version 0: Jane Coates 13/11/2015

use strict;
use diagnostics;
use Cwd qw( cwd );

die "Need date" if (@ARGV == 0);
my $date = $ARGV[0];
my @variables = qw( NOx.Emissions Temperature O3 Acetone );

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
    my ($nox_emission, $temperature, $acetone) = extract_data($checkfile, \@variables);
    
    for (0..$#$nox_emission) {
        print $out "$mechanism,$nox_emission->[$_],$temperature->[$_],$acetone->[$_]\n";
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

    my (@nox_emissions, @temperature, @acetone);
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
                    (my $temperature = $line) =~ s/^T_(.*?)_NOSF(.*?)$/$1/;
                    push @temperature, $temperature;
                }
            }
        } else {
            foreach my $line (@lines) {
                if ($line =~ / $variable /) {
                    if ($variable eq "Acetone") { 
                        push @acetone, get_value($line) * 1e9;
                    } else {
                        print "Missing data structure for $variable\n" unless ($variable eq "NOx");
                    }
                }
            }
        }
    }
    return \@nox_emissions, \@temperature, \@acetone; 
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
