#! /usr/bin/env perl
# Get VOC initial oxidation reaction rates. Mechanism is ARG
# Version 0: Jane Coates 16/2/2016
# Version 1: Jane Coates 18/2/2016 getting VOC emission rates, paths to Seagate data

use strict;
use diagnostics;
use MECCA;
use KPP;
use PDL;
use PDL::NiceSlice;

my $mechanism = $ARGV[0];
die "No mechanism specified! : $!" if (@ARGV == 0);

my $base = "/media/Seagate/Model_Data/T_Dependent_BVOCs/$mechanism/Temperature";

my($day, $month, $year) = (localtime)[3,4,5];
$month = sprintf '%02d', $month + 1;
$day   = sprintf '%02d', $day;
$year = sprintf '%04d', $year + 1900;
my $out_file = $mechanism . "_initial_VOC_oxidation_rate_" . $day . $month . $year . ".txt";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism,Temperature,HNO3,H2O2,VOC,Oxidation.Rate,Emission.Rate\n";

my $eqn = "/media/Seagate/Model_Data/T_Dependent_BVOCs/$mechanism/Trials/Benelux/gas.eqn";
my $kpp = KPP->new($eqn);
my $mecca = MECCA->new("$base/T_288_NOSF_0.01");
my $ntime = $mecca->time->nelem;
my $dt = $mecca->dt->at(0);
my $N_PER_DAY = 43200 / $dt;
my $N_DAYS = int $ntime / $N_PER_DAY;

chdir $base;
my @dirs = glob "T_*"; 
foreach my $dir (@dirs) {
    chdir "$base/$dir";
    (my $temperature = $dir) =~ s/T_(.*?)_NOSF(.*?)$/$1/;
    
    my $mecca = MECCA->new("$base/$dir"); 
    my @species = qw( HNO3 H2O2 );
    my %mixing_ratios;
    foreach my $species (@species) {
        $mixing_ratios{$species} = get_mixing_ratio($mecca, $mechanism, $species);
    }
    my ($oxidation, $emissions) = get_rates($kpp, $mecca);
    foreach my $VOC (sort keys %$emissions) {
        print $out "$mechanism,$temperature,$mixing_ratios{'HNO3'},$mixing_ratios{'H2O2'},$VOC,$oxidation->{$VOC},$emissions->{$VOC}\n";
    }
}
close $out;

sub get_rates {
    my ($kpp, $mecca) = @_;

    # get emitted VOCs
    my %emission;
    my $consumers = $kpp->consuming("UNITY");
    foreach my $reaction (@$consumers) {
        my $products = $kpp->products($reaction);
        next if ($products->[0] eq "NO");
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number);
        next if ($rate->sum == 0);
        $emission{$products->[0]} += $rate;
    }

    my %oxidation;
    foreach my $VOC (sort keys %emission) {
        my $consumers = $kpp->consuming($VOC);
        unless ($mechanism eq "RADM2") {
            die "No reactions found for $VOC\n" if (@$consumers == 0) ; 
        }
        foreach my $reaction (@$consumers) {
            my $rnum = $kpp->reaction_number($reaction);
            my $rate = $mecca->rate($rnum);
            next if ($rate->sum == 0);
            $oxidation{$VOC} += $rate
        }
    }
    foreach my $VOC (sort keys %oxidation) {
        my $reshape = $oxidation{$VOC}->reshape($N_PER_DAY, $N_DAYS);
        my $first_day = $reshape->sumover;
        $oxidation{$VOC} = $first_day->at(0);

        $reshape = $emission{$VOC}->reshape($N_PER_DAY, $N_DAYS);
        $first_day = $reshape->sumover;
        $emission{$VOC} = $first_day->at(0);
    }
    return (\%oxidation, \%emission);
}

sub get_mixing_ratio {
    my ($mecca, $mechanism, $species) = @_; 

    my $mixing_ratio = 0;
    $mixing_ratio = $mecca->tracer($species);
    my $reshape = $mixing_ratio->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day = $reshape(0)->xchg(0,1);
    return $first_day->max;
}
