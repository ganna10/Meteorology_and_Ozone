#! /usr/bin/env perl
# Get HOx production budget allocated to different types of HOx species. Mechanism is ARG
# Version 0: Jane Coates 7/12/2015

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
my $HOx_file = $mechanism . "_HOxSpecies.txt";
my @HOx = get_group($HOx_file);
$families{"HOx"} = [ @HOx ];

my $spc = "HOx";
my ($producers, $producer_yields);
if (exists $families{$spc}) {
    $kpp->family({
            name    => $spc,
            members => $families{$spc},
            weights => $weights{$spc},
    });
    $producers = $kpp->producing($spc);
    $producer_yields = $kpp->effect_on($spc, $producers);
    print "No producers for $spc found\n" if (@$producers == 0);
} else {
    print "No $spc family\n";
}

my($day, $month, $year) = (localtime)[3,4,5];
$month = sprintf '%02d', $month + 1;
$day   = sprintf '%02d', $day;
$year = sprintf '%04d', $year + 1900;
my $out_file = "${mechanism}_HOx_Budget_" . $day . $month . $year . ".txt";
open my $out, '>:encoding(utf-8)', $out_file or die $!;
print $out "Mechanism,Temperature,H2O2,HNO3,Reaction,Rate\n";
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
    
    my %production_reaction_rate;
    my $others = 1e7;
    for (0..$#$producers) {
        my $reaction = $producers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
        next if ($rate->sum == 0); 
        if ($rate->sum < $others) {
            $production_reaction_rate{"Others"} += $rate(12:24);
        } else {
            my $reaction_string = $kpp->reaction_string($reaction);
            $production_reaction_rate{$reaction_string} += $rate(12:24);
        }
    }
    my $temperature = 293;
#    foreach (keys %production_reaction_rate) {
#        if ($production_reaction_rate{$_}->sum < $others) {
#            $production_reaction_rate{"Others"} += $production_reaction_rate{$_};
#            delete $production_reaction_rate{$_};
#        }
#    }
    foreach my $reaction (sort keys %production_reaction_rate) {
        print $out "$mechanism,$temperature,$h2o2,$hno3,$reaction,", $production_reaction_rate{$reaction}->average, "\n";
    }
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
