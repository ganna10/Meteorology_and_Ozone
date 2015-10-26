#! /usr/bin/env perl
# Plot initial NMVOC conditions as percentage at each temperature, comparing between temperature dep and indep.
# Version 0: Jane Coates 26/10/2015

use strict;
use diagnostics;

my $temp_indep_file = "/local/home/coates/Documents/Variable_conditions/MCM_EMIS.nml";
my %NMVOC_emissions;

open my $in, '<:encoding(utf-8)', $temp_indep_file or die $!;
my @lines = <$in>;
close $in;

foreach my $line (@lines) {
    next unless ($line =~ /=/);
    chomp $line;
    my ($species, $emissions) = split /=/, $line;
    $species =~ s/EMIS_//;
    $emissions =~ s/,//;
    my $map = map_spc($species);
    $NMVOC_emissions{"T-Independent"}{$map} += $emissions;
}

foreach my $species (keys %{$NMVOC_emissions{"T-Independent"}}) {
    next if ($species eq "Isoprene");
    $NMVOC_emissions{"T-Dependent"}{$species} = $NMVOC_emissions{"T-Independent"}{$species};
}

my $file = "Mapped_emissions.csv";
open my $out, '>:encoding(utf-8)', $file or die $!;
print $out "Run,Species,Emissions\n";
foreach my $run (sort keys %NMVOC_emissions) {
    foreach my $mapping (sort keys %{$NMVOC_emissions{$run}}) {
        print $out "$run,$mapping,$NMVOC_emissions{$run}{$mapping}\n";
    }
}
close $out;

sub map_spc {
    my ($spc) = @_;
    my $mapping;
    if ($spc eq "C2H6") {
        $mapping = "Ethane";
    } elsif ($spc eq "C3H8") {
        $mapping = "Propane";
    } elsif ($spc eq "NC4H10" or $spc eq "IC4H10") {
        $mapping = "Butanes";
    } elsif ($spc eq "NC5H12" or $spc eq "IC5H12" or $spc eq "NEOP") {
        $mapping = "Pentanes";
    } elsif ($spc =~ /NC6|NC7|NC8|NC9|NC1\d|M\dPE|M\dHEX|M\d\dC4|CHEX/) {
        $mapping = "Higher Alkanes";
    } elsif ($spc eq "C2H4") {
        $mapping = "Ethene";
    } elsif ($spc eq "C3H6") {
        $mapping = "Propene";
    } elsif ($spc =~ /C2H2|\dENE|MEPROPENE|C4H6/) {
        $mapping = "Higher Alkenes and Alkynes";
    } elsif ($spc eq "BENZENE") {
        $mapping = "Benzene";
    } elsif ($spc =~ /^TOL/) {
        $mapping = "Toluene";
    } elsif ($spc =~ /XYL/) {
        $mapping = "Xylenes";
    } elsif ($spc =~ /^TM/) {
        $mapping = "Trimethylbenzenes";
    } elsif ($spc =~ /BENZ|DIME|STYRENE|PHENOL|5TOL|HTOL$/) {
        $mapping = "Other aromatics";
    } elsif ($spc =~ /HCOOH|CH3COOH|PROPACID|CH3CO2H|ACO2H/) {
        $mapping = "Acids";
    } elsif ($spc =~ /CH3OH|C2H5OH|PROPOL|BUTOL|BUT2OL|MIBKAOH|C6H5CH2OH|GLY$|PECOH|IPE.OH|BUOL|HEXOL|MBO/) {
        $mapping = "Alcohols";
    } elsif ($spc =~ /CH3OCH3|BUOX2ETOH|PR2OHMOX|2EOL|MO2EOL|ETHER|TBE$|PROL$/) {
        $mapping = "Ethers";
    } elsif ($spc =~ /ACET$|CH3OCHO/) {
        $mapping = "Esters";
    } elsif ($spc =~ /CH3CHO|C2H5CHO|C3H7CHO|IPRCHO|C4H9CHO|ACR|C4ALDB|HCHO|MGLYOX/) {
        $mapping = "Aldehydes";
    } elsif ($spc =~ /MEK|CH3COCH3|M.BK|ONE$|MPRK|DIEK|MI.K/) {
        $mapping = "Ketones";
    } elsif ($spc =~ /CLETH|CHCL2CH3|VINCL|CCL2CH2|TCE|CL\d?$/) {
        $mapping = "Chlorinated";
    } elsif ($spc eq "C5H8") {
        $mapping = "Isoprene";
    } elsif ($spc =~ /PINENE|LIMONENE/) {
        $mapping = "Terpenes";
    } else {
        print "No mapping for $spc\n";
    }
    return $mapping;
}
