#! /usr/bin/env perl
# testing script to get NOx and VOC emission rates
# Version 0: Jane Coates 29/8/2015

use strict;
use diagnostics;
use MECCA;
use KPP;
use PDL;

my $dir = "/local/home/coates/MECCA/MCMv3.2_tagged";
my $mecca = MECCA->new("$dir/boxmodel");
my $kpp = KPP->new("$dir/gas.eqn");

my @NOx = qw( NO NO2 );

foreach my $NOx (@NOx) {
    my $reactions = $kpp->producing_from($NOx, "UNITY");
    foreach my $reaction (@$reactions) {
        print $kpp->reaction_string($reaction), "\n";
    }
}
