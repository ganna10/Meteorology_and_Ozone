#! /usr/bin/env perl
# Assign emitted VOCs to groups
# Version 0: Jane Coates 7/6/2016

my @runs = ("TI", "TD");
my @mechanisms = qw( MCMv3.2 CRIv2 RADM2 MOZART-4 CB05 );
#my @mechanisms = qw( CB05 RADM2 MOZART-4 );
foreach my $run (@runs) {
    foreach my $mechanism (@mechanisms) {
        my $file = $run . "_VOCR/" . $mechanism . "_initial_VOC_OH_oxidation_rate_07062016.txt";
        open my $in, '<:encoding(utf-8)', $file or die "Can't open $file : $!";
        my @lines = <$in>;
        close $in;
        
        my $out_file = $run . "_VOCR/" . $mechanism . "_assigned_initial_VOC_OH_oxidation_rate_07062016.txt";
        open my $out, '>:encoding(utf-8)', $out_file or die "Can't open $out_file: $!";
        foreach my $line (@lines) {
            chomp $line;
            if ($line =~ /^Mechanism/) {
                $line .= ",Group\n";
                print $out $line;
                next;
            }
            my ($mechanism, $temperature, $o3, $hno3, $h2o2, $VOC, $max_vocr, $sum_vocr, $emission_rate) = split /,/, $line;
            my $group = get_group($VOC, $mechanism);
            $line .= ",$group\n";
            print $out $line;
        }
        close $out;
    }
}

sub get_group {
    my ($VOC, $mechanism) = @_;
    my $group;

    if ($VOC eq "ETHA") {
        $group = "Ethane";
    } elsif ($VOC eq "C2H6") {
        $group = "Ethane";
    } elsif ($VOC eq "ETH" and $mechanism eq "RADM2") {
        $group = "Ethane";
    } elsif ($VOC eq "C3H8" or $VOC eq "HC3") {
        $group = "Propane";
    } elsif ($VOC =~ /(N|I)C4H10/) {
        $group = "Butanes";
    } elsif ($VOC =~ /(N|I)C5H12/ or $VOC eq "NEOP" or $VOC eq "HC5" or $VOC eq "BIGALK") {
        $group = "Pentanes";
    } elsif ($VOC =~ /NC([6-9]|1[0-2])H/ or $VOC =~ /M(2|3)(PE|HEX)/ or $VOC =~ /M2(2|3)C4/ or $VOC eq "CHEX" or $VOC eq "HC8" or $VOC eq "PAR") {
        $group = "Higher and Other Alkanes";
    } elsif ($VOC eq "ETH" or $VOC eq "C2H4" or $VOC eq "OL2") {
        $group = "Ethene";
    } elsif ($VOC eq "C3H6" or $VOC eq "OLE") {
        $group = "Propene";
    } elsif ($VOC =~ /(A|B)PINENE/ or $VOC eq "LIMONENE" or $VOC eq "C10H16" or $VOC eq "TERP") {
        $group = "Monoterpenes";
    } elsif ($VOC eq "OLT" or $VOC eq "OLI" or $VOC eq "BIGENE" or $VOC =~ /\dENE$/ or $VOC eq "MEPROPENE" or $VOC eq "C4H6" or $VOC eq "IOLE") {
        $group = "Higher and Other Alkenes";
    } elsif ($VOC eq "C2H2") {
        $group = "Ethyne";
    } elsif ($VOC eq "BENZENE") {
        $group = "Benzene";
    } elsif ($VOC eq "TOL" or $VOC eq "TOLUENE") {
        $group = "Toluene";
    } elsif ($VOC =~ /XYL/) {
        $group = "Xylenes";
    } elsif ($VOC =~ /TM1(2|3)[3-5]B/) {
        $group = "Trimethylbenzenes";
    } elsif ($VOC =~ /(H|5)TOL$|BENZ$|35EB$/ or $VOC eq "STYRENE" or $VOC eq "BENZAL" or $VOC eq "PHENOL" or $VOC eq "AROH14" or $VOC eq "CSL") {
        $group = "Other Aromatics";
    } elsif ($VOC eq "HCHO" or $VOC eq "CH2O" or $VOC eq "FORM") {
        $group = "Formaldehyde";
    } elsif ($VOC eq "CH3OCHO") {
        $group = "Esters";
    } elsif ($VOC =~ /ALD(2|X)/ or $VOC eq "ALD" or $VOC eq "MGLY" or $VOC eq "MACR" or $VOC =~ /CHO$/ or $VOC =~ /CARB/ or $VOC eq "ACR" or $VOC eq "C4ALDB") {
        $group = "Aldehydes";
    } elsif ($VOC eq "C5H8" or $VOC =~ /^ISO/) { 
        $group = "Isoprene";
    } elsif ($VOC eq "HCOOH" or $VOC eq "CH3CO2H" or $VOC eq "PROPACID" or $VOC eq "ACO2H" or $VOC eq "CH3COOH" or $VOC =~ /ORA(1|2)/ or $VOC =~ /(F|A)ACD/) {
        $group = "Organic Acids";
    } elsif ($VOC =~ /(\d|[A-C]|E)OH$/ or $VOC =~ /(\d|P|T|X|U)OL$/ or $VOC =~ /(H|P)GLY$/ or $VOC eq "MBO" or $VOC eq "C6H5CH2OH" or $VOC eq "ETOH") {
        $group = "Alcohols";
    } elsif ($VOC eq "KET" or $VOC eq "MEK" or $VOC =~ /ONE$/ or $VOC eq "CH3COCH3" or $VOC =~ /(R|E|P|B)K$/) {
        $group = "Ketones";
    } elsif ($VOC eq "CH3OCH3" or $VOC =~ /ETHER/ or $VOC =~ /BE$/ or $VOC =~ /(E|R)OL$|OX$|ETOH/) {
        $group = "Ethers";
    } elsif ($VOC =~ /CLETH/ or $VOC =~ /CL/ or $VOC eq "TCE") {
        $group = "Chlorinated";
    } elsif ($VOC =~ /ACET$/) {
        $group = "Esters";
    } else {
        $group = $VOC;
    }
    return $group;
}
