#! /usr/bin/env perl
# Get Ox = O3 + NO2 budget. Mechanism is ARGUMENT
# Version 0: Jane Coates 2/12/2015

use strict;
use diagnostics;
use KPP;
use MECCA;
use PDL;
use PDL::NiceSlice;

die "Specify mechanism!" if (@ARGV == 0);
my $mechanism = $ARGV[0];

my $benelux_base = "/local/home/coates/Variable_Conditions/$mechanism/Benelux";
#my $benelux_base = "/work/users/jco/EU_Average_Sensitivity/T_Dependent_BVOCs/$mechanism/Trials/Benelux";
my $kpp = KPP->new("$benelux_base/gas.eqn");
my (%families, %weights, %production_budget);
$families{"Ox"} = [ qw( O3 NO2 O ) ];
my @NOx = qw( NO NO2 );

# define allocatable groups
my $acyl_file = $mechanism . "_acylRO2.txt";
my @ARO2 = get_group($acyl_file);
my $nonacyl_file = $mechanism . "_nonacylRO2.txt";
my @RO2 = get_group($nonacyl_file);
my @inorganic = qw( O O1D O3P NO NO2 NO3 N2O5 HNO3 HONO OH HO2 );
my $ro2no2_file = $mechanism . "_peroxynitrates.txt";
my @RO2NO2 = get_group($ro2no2_file);

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
print $out "Mechanism,Temperature,H2O2,HNO3,Reaction,Rate,Net.Reaction.Rate\n";

#my $base = "/local/data/jco/$mechanism";

#my $mecca = MECCA->new("$base/T_288_NOSF_0.01");
my $mecca = MECCA->new("$benelux_base/boxmodel");
my $ntime = $mecca->time->nelem;
my $dt = $mecca->dt->at(0);
my $N_PER_DAY = 43200 / $dt;
my $N_DAYS = int $ntime / $N_PER_DAY;

#chdir $base;
#my @dirs = glob "T_*";
#foreach my $dir (@dirs) {
#chdir "$base/$dir";
#(my $temperature = $dir) =~ s/T_(.*?)_NOSF_(.*?)$/$1/;
#my $mecca = MECCA->new("$base/$dir");
    
    #my $nox_mr = get_mixing_ratio($mecca, $mechanism, "NOx"); #max first day
    #my $nox_emissions = get_emissions($mecca, $kpp, \@NOx); #sum first day
    my $h2o2 = get_mixing_ratio($mecca, $mechanism, "H2O2");
    my $hno3 = get_mixing_ratio($mecca, $mechanism, "HNO3");
    
    my $cair = $mecca->cair;
    my (%production_reaction_rate, %consumption_reaction_rate, $net_rate);
    for (0..$#$producers) {
        my $reaction = $producers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
        next if ($rate->sum == 0); 
        my $reaction_string = $kpp->reaction_string($reaction);
        my ($reactants, $products) = split / = /, $reaction_string;
        $net_rate += $rate;
        my $category = get_category($reactants);
        $production_reaction_rate{$category} += $rate/$cair * 1e9 * (60*60);
    }

    for (0..$#$consumers) {# just to calculate net production
        my $reaction = $consumers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $consumer_yields->[$_];
        next if ($rate->sum == 0); 
        $net_rate += $rate;
    }
    
    foreach (keys %production_reaction_rate) {
        my $reshape = $production_reaction_rate{$_}->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
        $reshape = $reshape(0)->xchg(0,1);
        $production_reaction_rate{$_} = $reshape->max;
    }

    $net_rate = $net_rate->sum;
    foreach my $reaction (sort keys %production_reaction_rate) {
        print $out "$mechanism,293,$h2o2,$hno3,$reaction,$production_reaction_rate{$reaction},$net_rate\n";
    }
#}

close $out;

sub get_all_emissions {
    my ($mecca, $kpp) = @_;
    my $emission_rate; 
    my $reactions = $kpp->consuming("UNITY");
    foreach my $reaction (@$reactions) {
        my $products = $kpp->products($reaction);
        next if ($products->[0] =~ /NO/);
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number);
        $emission_rate += $rate;
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

sub get_group {
    my ($file) = @_;
    open my $in, '<:encoding(utf-8)', $file or die $!;
    my @lines = <$in>;
    close $in;
    return grep { chomp $_ } @lines;
}

sub get_category {
    my ($reactants) = @_;
    my $category;
    $reactants =~ s/ \+ hv//;
    if ($reactants eq "HO2 + NO") {
        $category = "HO2";
    } elsif ($reactants !~ /\+/) {
        if ($reactants ~~ @inorganic) {
            $category = $reactants;
            #$category = "Inorganic";
        } elsif ($reactants ~~ @RO2NO2) {
            $category = "RO2NO2";
        } else {
            $category = "Other Organic";
            print "Other Organic : $reactants\n";
        }
    } elsif ($reactants =~ /\+/) {
        my ($first, $second) = split / \+ /, $reactants;
        if ($first ~~ @ARO2 or $second ~~ @ARO2) {
            $category = "ARO2";
        } elsif ($first ~~ @RO2 or $second ~~  @RO2) {
            $category = "RO2";
        } elsif ($first ~~ @inorganic and $second ~~ @inorganic) {
            $category = $reactants;
            #$category = "Inorganic";
        } elsif ($first ~~ @RO2NO2 or $second ~~ @RO2NO2) {
            $category = "RO2NO2";
        } else {
            $category = "Other Organic";
            print "Other Organic : $reactants\n";
        }
    }
    return $category;
}
