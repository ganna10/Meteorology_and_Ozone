#! /usr/bin/env perl
# Get values of variables need to compare box model to Pusede:2014 analytical model. Mechanism is ARGUMENT
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

# define families
$families{"Ox"} = [ qw( O3 NO2 ) ];
my $AN_file = $mechanism . "_alkylnitrates.txt";
my @ANs = get_group($AN_file);
$families{"AN"} = [ @ANs ];
my $HOx_file = $mechanism . "_HOxSpecies.txt";
my @HOx = get_group($HOx_file);
$families{"HOx"} = [ @HOx ];

my @families = ("Ox", "AN", "HOx");
my %family_info;
foreach my $spc (@families) {
    if (exists $families{$spc}) {
        $kpp->family({
                name    => $spc,
                members => $families{$spc},
                weights => $weights{$spc},
        });
        my $producers = $kpp->producing($spc);
        print "No producers for $spc found\n" if (@$producers == 0);
        
        $family_info{$spc}{"Producers"} = $producers;
        $family_info{$spc}{"Producer_Yields"} = $kpp->effect_on($spc, $producers);
    } else {
        print "No $spc family\n";
    }
}

my($day, $month, $year) = (localtime)[3,4,5];
$month = sprintf '%02d', $month + 1;
$day   = sprintf '%02d', $day;
$year = sprintf '%04d', $year + 1900;
my $out_file = "${mechanism}_analytical_model_" . $day . $month . $year . ".txt";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism,Temperature,H2O2,HNO3,NO,NO2,VOCR,PHOx,alpha\n";
#
##my $base = "/local/data/jco/$mechanism";
#
##my $mecca = MECCA->new("$base/T_288_NOSF_0.01");
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
    
    my $h2o2 = get_mixing_ratio($mecca, $mechanism, "H2O2");
    my $hno3 = get_mixing_ratio($mecca, $mechanism, "HNO3");
    my $no = get_mixing_ratio($mecca, $mechanism, "NO");
    my $no2 = get_mixing_ratio($mecca, $mechanism, "NO2");
    my $vocr = get_vocr($kpp, $mecca);
    
    my %production_reaction_rate;
    foreach my $spc (keys %family_info) {
        my $producers = $family_info{$spc}{"Producers"};
        my $producer_yields = $family_info{$spc}{"Producer_Yields"};
        for (0..$#$producers) {
            my $reaction = $producers->[$_];
            my $reaction_number = $kpp->reaction_number($reaction);
            my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
            next if ($rate->sum == 0); 
            $production_reaction_rate{$spc} += $rate;
        }
    }
    foreach (keys %production_reaction_rate) {
        my $reshape = $production_reaction_rate{$_}->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
        $reshape = $reshape(0)->xchg(0,1);
        $production_reaction_rate{$_} = $reshape->sum;
    }
    my $alpha =  2 * $production_reaction_rate{"AN"} / $production_reaction_rate{"Ox"};
    my $temperature = 293;
    print $out "$mechanism,$temperature,$h2o2,$hno3,$no,$no2,$vocr,$production_reaction_rate{'HOx'},$alpha\n";
##}

close $out;

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

sub get_vocr {
    my ($kpp, $mecca) = @_;

    my $reactant = 'OH';
    my $cair = $mecca->cair;
    my $consumers = $kpp->consuming($reactant);
    die "No reactions found for $reactant\n" if (@$consumers == 0) ;

    my @non_spcs = qw( NO NO2 HONO HNO3 NH3 SO2 );
    my $total_reactivity;
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
    }
    my $reshape = $total_reactivity->reshape($N_PER_DAY, $N_DAYS)->xchg(0,1);
    my $first_day = $reshape(0)->xchg(0,1);
    return $first_day->max;
}
