#! /usr/bin/env perl
# Get O3 net budget. Mechanism is ARGUMENT
# Version 0: Jane Coates 05/11/2015
# Version 1: Jane Coates 10/11/2015 full definition of Ox family
# Version 0: Jane Coates 17/11/2015 removing sorting of hash

use strict;
use diagnostics;
use KPP;
use MECCA;
use PDL;
use PDL::NiceSlice;

die "Specify mechanism!" if (@ARGV == 0);
my $mechanism = $ARGV[0];

my $benelux_base = "/local/home/coates/Variable_Conditions/$mechanism/Benelux";
my $ro2_file = "$benelux_base/RO2_species.txt";
my $kpp = KPP->new("$benelux_base/gas.eqn");
my @no2_reservoirs = get_no2_reservoirs($kpp, $ro2_file);
my (%families, %weights, %production_budget);
$families{"Ox"} = [ qw( O3 NO2 O O1D NO3 N2O5 HO2NO2 ), @no2_reservoirs ];
$weights{"Ox"} = { NO3 => 2, N2O5 => 3 };
my @NOx = qw( NO NO2 );

my ($producers, $consumers, $producer_yields, $consumer_yields);
if (exists $families{"Ox"}) {
    $kpp->family({
            name    => "Ox",
            members => $families{"Ox"},
            weights => $weights{"Ox"},
    });
    $producers = $kpp->producing("Ox");
    $producer_yields = $kpp->effect_on("Ox", $producers);
    $consumers = $kpp->consuming("Ox");
    $consumer_yields = $kpp->effect_on("Ox", $consumers);
} else {
    print "No Ox family\n";
}
print "No producers for Ox found\n" if (@$producers == 0);
print "No consumers for Ox found\n" if (@$consumers == 0);

my($day, $month, $year) = (localtime)[3,4,5];
$month = sprintf '%02d', $month + 1;
$day   = sprintf '%02d', $day;
$year = sprintf '%04d', $year + 1900;
my $out_file = "${mechanism}_O3_budget_" . $day . $month . $year . ".txt";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism,Temperature,H2O2,HNO3,NOx.Mixing.Ratio,NOx.Emissions,Reaction,Rate,Net.Reaction.Rate\n";

my $base = "/work/users/jco/EU_Average_Sensitivity/T_Dependent_BVOCs/$mechanism/Temperature";

my $mecca = MECCA->new("$base/T_288_NOSF_0.01");
my $ntime = $mecca->time->nelem;
my $dt = $mecca->dt->at(0);
my $N_PER_DAY = 43200 / $dt;
my $N_DAYS = int $ntime / $N_PER_DAY;

chdir $base;
my @dirs = glob "T_*";
foreach my $dir (@dirs) {
    chdir "$base/$dir";
    (my $temperature = $dir) =~ s/T_(.*?)_NOSF_(.*?)$/$1/;
    my $mecca = MECCA->new("$base/$dir");
    
    my $nox_mr = get_mixing_ratio($mecca, $mechanism, "NOx"); #max first day
    my $nox_emissions = get_emissions($mecca, $kpp, \@NOx); #sum first day
    my $h2o2 = get_mixing_ratio($mecca, $mechanism, "H2O2");
    my $hno3 = get_mixing_ratio($mecca, $mechanism, "HNO3");

    my (%production_reaction_rate, %consumption_reaction_rate, $net_production_rate);
    for (0..$#$producers) {
        my $reaction = $producers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
        next if ($rate->sum == 0);
        $net_production_rate += $rate;
        my $reaction_string = $kpp->reaction_string($reaction);
        $production_reaction_rate{$reaction_string} += $rate;
    }

    for (0..$#$consumers) {
        my $reaction = $consumers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $consumer_yields->[$_];
        next if ($rate->sum == 0);
        $net_production_rate += $rate;
        my $reaction_string = $kpp->reaction_string($reaction);
        $consumption_reaction_rate{$reaction_string} += $rate;
    } 

    #get net production of first day light period and sum of first day light periods in production and consumption rates
    my $net_reshape = $net_production_rate->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $net_first_day = $net_reshape(0)->xchg(0,1);
    $net_first_day = $net_first_day->sum;
    
    foreach (keys %production_reaction_rate) {
        my $reshape = $production_reaction_rate{$_}->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
        $reshape = $reshape(0)->xchg(0,1);
        $production_reaction_rate{$_} = $reshape->sum;
    }
    foreach (keys %consumption_reaction_rate) {
        my $reshape = $consumption_reaction_rate{$_}->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
        $reshape = $reshape(0)->xchg(0,1);
        $consumption_reaction_rate{$_} = $reshape->sum;
    }

    my $others_max = 1.5e7;
    foreach my $reactants (keys %production_reaction_rate) {
        if ($production_reaction_rate{$reactants} < $others_max) {
            $production_reaction_rate{"Production Others"} += $production_reaction_rate{$reactants};
            delete $production_reaction_rate{$reactants};
        }
    }
    #print "$_ : $production_reaction_rate{$_}\n" foreach sort keys %production_reaction_rate;
    foreach my $reactants (keys %consumption_reaction_rate) {
        if ($consumption_reaction_rate{$reactants} > -$others_max) {
            $consumption_reaction_rate{"Consumption Others"} += $consumption_reaction_rate{$reactants};
            delete $consumption_reaction_rate{$reactants};
        }
    }
    #print "$_ : $consumption_reaction_rate{$_}\n" foreach sort keys %consumption_reaction_rate;
    foreach my $reaction (sort keys %production_reaction_rate) {
        print $out "$mechanism,$temperature,$h2o2,$hno3,$nox_mr,$nox_emissions,$reaction,$production_reaction_rate{$reaction},$net_first_day\n";
    }
    foreach my $reaction (sort keys %consumption_reaction_rate) {
        print $out "$mechanism,$temperature,$h2o2,$hno3,$nox_mr,$nox_emissions,$reaction,$consumption_reaction_rate{$reaction},$net_first_day\n";
    }
}

close $out;

sub get_emissions {
    my ($mecca, $kpp, $spc, $VOC_carbons) = @_;
    my $emission_rate;
    foreach my $spc (@$spc) {
        my $reactions = $kpp->producing_from($spc, "UNITY");
        foreach my $reaction (@$reactions) {
            my $reaction_number = $kpp->reaction_number($reaction);
            my $rate = $mecca->rate($reaction_number);
            if (defined $VOC_carbons->{$spc}) {
                $rate *= $VOC_carbons->{$spc};
            }
            $emission_rate += $rate;
        }
    }
    my $reshape = $emission_rate->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day = $reshape(0)->xchg(0,1);
    return $first_day->sum;
}

sub get_mixing_ratio {
    my ($mecca, $mechanism, $species) = @_;

    my $mixing_ratio = 0;
    if ($species eq "NOx") {
        foreach my $item (@NOx) {
            my $mr = $mecca->tracer($item);
            $mixing_ratio += $mr;
        }
    } elsif ($species eq "HCHO") {
        if ($mechanism eq "CB05") {
            $mixing_ratio = $mecca->tracer("FORM");
        } elsif ($mechanism eq "MOZART-4") {
            $mixing_ratio = $mecca->tracer("CH2O");
        } else {
            $mixing_ratio = $mecca->tracer($species);
        }
    } else {
        $mixing_ratio = $mecca->tracer($species);
    } 
    my $reshape = $mixing_ratio->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day = $reshape(0)->xchg(0,1);
    return $first_day->max;
}

sub get_no2_reservoirs { #get species that are produced when radical species react with NO2             
    my ($kpp, $file) = @_; 
    open my $in, '<:encoding(utf-8)', $file or die $!; 
    my @ro2;
    for (<$in>) {
        push @ro2, split /\s+/, $_; 
    }   
    close $in;
    my @no2_reservoirs;
    foreach my $ro2 (@ro2) {
        my ($reactions) = $kpp->reacting_with($ro2, 'NO2');
        foreach my $reaction (@$reactions) {
            my ($products) = $kpp->products($reaction);
            if (@$products == 1) {
                push @no2_reservoirs, $products->[0];
            }
        }
    }   
    return @no2_reservoirs;
} 
