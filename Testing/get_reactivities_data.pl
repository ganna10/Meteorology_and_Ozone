#! /usr/bin/env perl
# Get OH reactivity of all organics, initial NMVOC and reaction rate of all RO2 + NO. For use as normalising data. Mechanism is ARGUMENT
# Version 0: Jane Coates 8/12/2015

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
my (%families, %weights, %family_data);

my @emitted_VOC = get_initial_voc($kpp);
$families{"VOC"} = [ @emitted_VOC ];

my $hox_file = "${mechanism}_HOxSpecies.txt";
my @HOx = get_group($hox_file);
my $oh = shift @HOx;
die "Not OH\n" unless ($oh eq "OH");
$families{"RO2x"} = [ @HOx ];

foreach my $spc (qw( RO2x VOC )) {
    my ($consumers, $producers, $producer_yields, $consumer_yields);
    if (exists $families{$spc}) {
        $kpp->family({
                name    => $spc,
                members => $families{$spc},
                weights => $weights{$spc},
        });
        $producers = $kpp->producing($spc); 
        $producer_yields = $kpp->effect_on($spc, $producers);
        $consumers = $kpp->consuming($spc); 
        $consumer_yields = $kpp->effect_on($spc, $consumers);
        $family_data{$spc}{"Producers"} = $producers;
        $family_data{$spc}{"Producer_Yields"} = $producer_yields;
        $family_data{$spc}{"Consumers"} = $consumers;
        $family_data{$spc}{"Consumer_Yields"} = $consumer_yields;
    } else {
        print "No $spc family\n";
    }
    print "No producers for $spc found\n" if (@$producers == 0);
    print "No consumers for $spc found\n" if (@$consumers == 0);
}

my($day, $month, $year) = (localtime)[3,4,5];
$month = sprintf '%02d', $month + 1;
$day   = sprintf '%02d', $day;
$year = sprintf '%04d', $year + 1900;
my $out_file = "${mechanism}_normalising_data_" . $day . $month . $year . ".txt";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism,Temperature,H2O2,HNO3,VOCR.total,VOCR.initial,VOC.initial.losses,RO2.NO,RO2x.production\n";

my $budget_out_file = "${mechanism}_RO2x_budget_" . $day . $month . $year . ".txt";
open my $budget_out, '>:encoding(utf-8)', $budget_out_file or die $!;
print $budget_out "Mechanism,Temperature,H2O2,HNO3,Reaction,RO2x.production\n";

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

my $temperature = "293";
    
    my $h2o2 = get_mixing_ratio($mecca, $mechanism, "H2O2");
    my $hno3 = get_mixing_ratio($mecca, $mechanism, "HNO3");
    my $vocr_initial_losses = get_voc_initial_losses($mecca, $kpp); #sum first day
    my ($total_vocr, $initial_vocr) = get_vocr($mecca, $kpp);
    my $ro2_NO_reactivity = get_ro2_no_reactivity($mecca, $kpp);
    print $out "$mechanism,$temperature,$h2o2,$hno3,$total_vocr,$initial_vocr,$vocr_initial_losses,$ro2_NO_reactivity\n";

    my $ro2x_production_data = get_ro2x_production($mecca, $kpp);
    foreach my $reaction (sort keys %$ro2x_production_data) {
        print $budget_out "$mechanism,$h2o2,$hno3,$reaction,$ro2x_production_data->{$reaction}\n";
    }
#}

close $out;
close $budget_out;

sub get_initial_voc {
    my ($kpp) = @_;
    my %VOC;
    my $reactions = $kpp->consuming("UNITY");
    my @non_match = qw( NO NO2 CH4 CO );
    foreach my $reaction (@$reactions) {
        my $products = $kpp->products($reaction);
        next if ($products->[0] ~~ @non_match);
        $VOC{$products->[0]}++;
    }
    return sort keys %VOC;
}

sub get_mixing_ratio {
    my ($mecca, $mechanism, $species) = @_;

    my $mixing_ratio = 0;
    if ($species eq "NOx") {
        foreach my $item (qw(NO NO2)) {
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

sub get_voc_initial_losses {
    my ($mecca, $kpp) = @_;
    my $item = "VOC";
    my $consumers = $family_data{$item}{"Consumers"};
    my $consumer_yields = $family_data{$item}{"Consumer_Yields"};
    my $total_loss_rate;
    for (0..$#$consumers) {
        my $reaction = $consumers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $consumer_yields->[$_];
        next if ($rate->sum == 0);
        $total_loss_rate += $rate * -1;
    } 
    my $reshape_loss = $total_loss_rate->reshape($N_PER_DAY, $N_DAYS)->xchg(0, 1);
    my $first_day_loss = $reshape_loss(0)->xchg(0, 1);
    return $first_day_loss->sum;
}

sub get_initial_vocr {
    my ($mecca, $kpp) = @_;
    return;
}

sub get_vocr {
    my ($mecca, $kpp) = @_;

    my $reactant = 'OH';
    my $cair = $mecca->cair;
    my $consumers = $kpp->consuming($reactant);
    die "No reactions found for $reactant\n" if (@$consumers == 0) ;

    my @non_spcs = qw( NO NO2 HONO HNO3 NH3 SO2 O3 H2 CO H2O2 HO2 NO3 HO2NO2 );
    my ($total_reactivity, $initial_reactivity);
    foreach my $reaction (@$consumers) {
        my $reactants = $kpp->reactants($reaction);
        my ($other_reactant) = grep { $_ ne $reactant } @$reactants;
        next if ($other_reactant ~~ @non_spcs);
        my $rnum = $kpp->reaction_number($reaction);
        my $rconst = $mecca->rconst($rnum);
        next if (isbad($rconst(-1)));
        my $other_reactant_conc = $mecca->tracer($other_reactant) * $cair;
        my $reactivity = $rconst * $other_reactant_conc;
        next if ($reactivity->sum == 0);
        $total_reactivity += $reactivity;

        if ($other_reactant ~~ @emitted_VOC) {
            $initial_reactivity += $reactivity;
        }
    }
    my $reshape_total = $total_reactivity->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day_total = $reshape_total(0)->xchg(0,1);
    my $reshape_initial = $initial_reactivity->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day_initial = $reshape_initial(0)->xchg(0,1);
    return ($first_day_total->sum, $first_day_initial->sum);
}

sub get_ro2_no_reactivity {
    my ($mecca, $kpp) = @_;
    my $ro2_no_reactivity;
    my $consumers = $family_data{"RO2x"}{"Consumers"};
    my $cair = $mecca->cair;
    foreach my $reaction (@$consumers) {
        my $reactants = $kpp->reactants($reaction);
        next unless ("NO" ~~ @$reactants);
        my ($other_reactant) = grep { $_ ne "NO" } @$reactants;
        my $rnum = $kpp->reaction_number($reaction);
        my $rconst = $mecca->rconst($rnum);
        next if (isbad($rconst(-1)));
        my $other_reactant_conc = $mecca->tracer($other_reactant) * $cair;
        my $reactivity = $rconst * $other_reactant_conc;
        next if ($reactivity->sum == 0);
        $ro2_no_reactivity += $reactivity;
    }
    my $reshape = $ro2_no_reactivity->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day = $reshape(0)->xchg(0,1);
    return $first_day->sum;
}

sub get_ro2x_production {
    my ($mecca, $kpp) = @_;

    my %production_reaction_rate;
    my $producers = $family_data{"RO2x"}{"Producers"};
    my $producer_yields = $family_data{"RO2x"}{"Producer_Yields"};
    my $others = 1e7;
    for (0..$#$producers) {
        my $reaction = $producers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
        next if ($rate->sum == 0); 
        if ($rate->sum < $others) {
            $production_reaction_rate{"Others"} += $rate;
        } else {
            my $reaction_string = $kpp->reaction_string($reaction);
            $production_reaction_rate{$reaction_string} += $rate;
        }
    }
    
    foreach (keys %production_reaction_rate) {
        my $reshape = $production_reaction_rate{$_}->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
        $reshape = $reshape(0)->xchg(0,1);
        $production_reaction_rate{$_} = $reshape->sum;
    }
    return \%production_reaction_rate;
}
