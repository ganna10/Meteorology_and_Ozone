#! /usr/bin/env perl
# extract NOx, VOC and O3 data from check file, for each mechanism and output to csv file. Date (ddmmyyyy) is argument
# Version 0: Jane Coates 27/8/2015

use strict;
use diagnostics;

die "No date of check file" if (@ARGV == 0);
my $checkfile = "check_" . $ARGV[0] . ".txt";
print "Extracting data from $checkfile\n";

open my $in, '<:encoding(utf-8)', $checkfile or die $!;
local $/ = undef; #slurp mode
my $all_lines = <$in>;
close $in;

my @data;
push @data, "Mechanism,NOx,VOC,O3,VOCR";
#my ($mcm_data) = $all_lines =~ /MCM(.*?)MOZ/s;
#my @mcm_data = extract_data($mcm_data, "MCM");
#push @data, @mcm_data;

my ($mozart_data) = $all_lines =~ /MOZART(.*?)$/s;
my @mozart_data = extract_data($mozart_data, "MOZART");
push @data, @mozart_data;

my $out_file = "out_" . $ARGV[0] . ".csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "$_\n" foreach @data;
close $out;

sub extract_data {
    my ($lines, $mechanism) = @_;
    my @lines = split /\n/, $lines;
    my (@sub_data, @nox, @voc, @vocr, @o3);
    foreach my $line (@lines) {
         next unless ($line =~ /Max/);
         if ($line =~ /NOx/) {
             push @nox, get_value($line);
         } elsif ($line =~ /VOCR/) {
             push @vocr, get_value($line);
         } elsif ($line =~ /VOC/) {
             push @voc, get_value($line);
         } elsif ($line =~ /O3/) {
             push @o3, get_value($line);
         }
    }

    for (0..@nox-1) {
        push @sub_data, "$mechanism,$nox[$_],$voc[$_],$o3[$_],$vocr[$_]";
    }
    return @sub_data;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    my $return = sprintf "%.5f", $number;
    return $return;
}
