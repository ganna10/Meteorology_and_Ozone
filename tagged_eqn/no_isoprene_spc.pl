#! /usr/bin/env perl
# Get number of species needed in each mechanism to fully describe isoprene degradation
# Version 0: Jane Coates 26/11/2015

use strict;
use diagnostics;
use KPP;

my @mechanisms = qw( cb05 cri mcm mozart radm2 );
my %species;
foreach my $mechanism (@mechanisms) {
    my $eqn = $mechanism . ".eqn";
    my $kpp = KPP->new($eqn);
    my $reactions = $kpp->all_reactions();
    foreach my $reaction (@$reactions) {
        next unless ($reaction =~ /ISO|C5H8/);
        my $reactants = $kpp->reactants($reaction);
        foreach (@$reactants) { 
            if ($_ =~ /_/) {
                my ($item, $parent) = split /_/, $_;
                $species{$mechanism}{$item}++;
            }
        }
    }
}

#foreach my $mechanism (sort keys %species) {
#    my $no_of_spc = keys %{$species{$mechanism}};
#    print "$mechanism => $no_of_spc\n";
#    foreach my $spc (sort keys %{$species{$mechanism}}) {
#        #print "\t$spc\n";
#    }
#}
