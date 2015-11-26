#! /usr/bin/env perl

use strict;
use diagnostics;

my @mechanisms = qw( cb05 cri mcm mozart radm2 );
foreach my $mechanism (@mechanisms) {
    print "$mechanism\n";
    my $spc_file = "${mechanism}.spc";
    open my $in, '<:encoding(utf-8)', $spc_file or die $!;
    my @lines = <$in>;
    close $in;
    my %spcs;
    foreach my $line (@lines) {
        next unless ($line =~ /_/);
        my ($tagged, $rest) = split / = /, $line;
        my ($spc, $parent) = split /_/, $tagged;
        $spcs{$parent}{$spc}++;
    }
    my $isop;
    if ($mechanism =~ /mc|^c/) {
        $isop = "C5H8";
    } elsif ($mechanism =~ /mo/) {
        $isop = "ISOP";
    } else {
        $isop = "ISO";
    }
    my @isops = sort keys %{$spcs{$isop}};
    delete $spcs{$isop};
    my %matches;
    foreach my $spc (@isops) {
        print "\t$spc\n";
        foreach my $parent (sort keys %spcs) {
            my @matches = keys %{$spcs{$parent}};
            print "\t\t$parent : $spc\n" if ($spc ~~ @matches);
            $matches{$spc}++ if ($spc ~~ @matches);
        }
    }
    print "No Isoprene spc : ", scalar @isops, "\n";
    print "Matches with other parents : ", scalar keys %matches, "\n";
}
