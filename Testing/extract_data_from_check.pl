#! /usr/bin/env perl
# extract NOx, VOC and O3 data from check file, for each mechanism and output to csv file. Variable and Date (ddmmyyyy) are arguments
# Version 0: Jane Coates 27/8/2015

use strict;
use diagnostics;

die "Need Variable and date" if (@ARGV == 0);
my $variable = $ARGV[0];
my $date = $ARGV[1];
my $checkfile = "check_" . $variable . "_" . $date . ".txt";
print "Extracting data from $checkfile\n";

open my $in, '<:encoding(utf-8)', $checkfile or die $!;
local $/ = undef; #slurp mode
my $all_lines = <$in>;
close $in;

my @data;
push @data, "Mechanism,NOx,VOC,O3,HNO3,H2O2,HO2,OH,HCHO,VOCR,NOx.Emissions,VOC.Emissions,Temperature,Relative.Humidity,Solar.Zenith.Angle";

#my ($mozart_data) = $all_lines =~ /MOZART(.*?)MCM/s;
my ($mozart_data) = $all_lines =~ /MOZART(.*?)$/s;
my @mozart_data = extract_data($mozart_data, "MOZART");
push @data, @mozart_data;

#my ($mcm_data) = $all_lines =~ /MCM(.*?)$/s;
#my @mcm_data = extract_data($mcm_data, "MCM");
#push @data, @mcm_data;

my $out_file = "out_${variable}_${date}.csv";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "$_\n" foreach @data;
close $out;

sub extract_data {
    my ($lines, $mechanism) = @_;
    my @lines = split /\n/, $lines;
    my (@sub_data, @nox, @voc, @vocr, @o3, @hno3, @h2o2, @ho2, @oh, @hcho, @nox_emissions, @voc_emissions, @temperature, @relative_humidity,@sza);
    foreach my $line (@lines) {
         if ($line =~ /NO_scaling/) { 
             (my $temperature = $line) =~ s/^(.*?)_T//;
             $temperature =~ s/VOC_(.*?)$//;
             $temperature =~ s/_//g;
             push @temperature, $temperature;
             (my $RH = $line) =~ s/^(.*?)_RH//;
             $RH =~ s/SZA_(.*?)$//;
             $RH =~ s/_//g;
             push @relative_humidity, $RH;
             (my $sza = $line) =~ s/^(.*?)_SZA//;
             $sza =~ s/T_(.*?)$//;
             $sza =~ s/_//g;
             push @sza, $sza;
         } elsif ($line =~ /Max NOx/) {
             push @nox, get_value($line);
         } elsif ($line =~ /VOCR/) {
             push @vocr, get_value($line);
         } elsif ($line =~ /Max VOC/) {
             push @voc, get_value($line);
         } elsif ($line =~ / O3 /) {
             push @o3, get_value($line);
         } elsif ($line =~ / HNO3 /) {
             push @hno3, get_value($line) * 1e9;
         } elsif ($line =~ / H2O2 /) {
             push @h2o2, get_value($line) * 1e9;
         } elsif ($line =~ / HO2 /) {
             push @ho2, get_value($line) * 1e12;
         } elsif ($line =~ / OH /) {
             push @oh, get_value($line) * 1e12;
         } elsif ($line =~ / HCHO /) {
             push @hcho, get_value($line) * 1e9;
         } elsif ($line =~ /NOx emissions/) {
             push @nox_emissions, get_value($line);
         } elsif ($line =~ /VOC emissions/) {
             push @voc_emissions, get_value($line);
         }
    }

    for (0..@nox-1) {
        push @sub_data, "$mechanism,$nox[$_],$voc[$_],$o3[$_],$hno3[$_],$h2o2[$_],$ho2[$_],$oh[$_],$hcho[$_],$vocr[$_],$nox_emissions[$_],$voc_emissions[$_],$temperature[$_],$relative_humidity[$_],$sza[$_]";
    }
    return @sub_data;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    return $number;
}
