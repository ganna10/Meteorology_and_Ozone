#! /usr/bin/env perl
# Get O3 production in ppbv/hr
# Jane Coates 30710/2015
#
use strict;
use diagnostics;
use MECCA;
use KPP;
use PDL;
use PDL::NiceSlice;

my $dir = "/local/home/coates/Solvent_Emissions/RADM2/TNO_Solvents_Only";
my $mecca = MECCA->new("$dir/boxmodel");
my $kpp = KPP->new("$dir/gas.eqn");

my $spc = "O3";
my $producers = $kpp->producing($spc);
my $producer_yields = $kpp->effect_on($spc, $producers);

my $production_rate;
for (0..$#$producers) {
    my $reaction = $producers->[$_];
    my $reaction_number = $kpp->reaction_number($reaction);
    my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
    next if ($rate->sum == 0);
    $production_rate += $rate; #molecules cm-3 s-1
}

my $cair = $mecca->cair;
my $conversion = $cair / (60 * 24 * 1e9); #convert to ppb /hr
print $production_rate / $conversion, "\n";
