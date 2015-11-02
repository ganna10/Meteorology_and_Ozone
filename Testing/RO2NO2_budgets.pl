#! /usr/bin/env perl
# look at RO2NO2 budgets in Trial runs of each mechanism, Temp independent
# Version 0: Jane Coates 02/11/2015

use strict;
use diagnostics;
use PDL;
use PDL::NiceSlice;
use Statistics::R;
use KPP;
use MECCA;

my $base = "/local/home/coates/Variable_Conditions/T_Independent";
my @mechanisms = qw( CB05 RADM2 MCMv3.2 MOZART-4 CRIv2 );
my (%families, %weights, %data);

my $mecca = MECCA->new("$base/CB05_Trial/boxmodel");
my $ntime = $mecca->time->nelem;
my $dt = $mecca->dt->at(0);
my $n_per_day = 43200 / $dt;
my $n_days = int $ntime / $n_per_day;

foreach my $mechanism (@mechanisms) {
    my $dir = "$base/${mechanism}_Trial";
    my $mecca = MECCA->new("$dir/boxmodel");
    my $kpp = KPP->new("$dir/gas.eqn");
    $families{"RO2NO2"} = get_ro2no2($mechanism);

    $kpp->family({
            name    => "RO2NO2",
            members => $families{"RO2NO2"},
            weights => $weights{"RO2NO2"},
    });
    my $producers = $kpp->producing("RO2NO2");
    my $producer_yields = $kpp->effect_on("RO2NO2", $producers);
    my $consumers = $kpp->consuming("RO2NO2");
    my $consumer_yields = $kpp->effect_on("RO2NO2", $consumers);
    print "No consumers for RO2NO2\n" if (@$consumers == 0);
    print "No producers for RO2NO2\n" if (@$producers == 0); 
    my %reaction_rates;
    
    for (0..$#$producers) {
        my $reaction = $producers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $producer_yields->[$_];
        next if ($rate->sum == 0);
        my $reaction_string = $kpp->reaction_string($reaction);
        if ($mechanism eq "MCMv3.2" or $mechanism eq "CRIv2") {
            unless ($reaction_string =~ /\bPAN|PPN\b/) {
                $reaction_string = "Production Others";
            }
        }
        my $reshape = $rate->reshape($n_per_day, $n_days);
        my $integrate = $reshape->sumover;
        $reaction_rates{$reaction_string} += $integrate->at(0);
    }

    for (0..$#$consumers) {
        my $reaction = $consumers->[$_];
        my $reaction_number = $kpp->reaction_number($reaction);
        my $rate = $mecca->rate($reaction_number) * $consumer_yields->[$_];
        next if ($rate->sum == 0);
        my $reaction_string = $kpp->reaction_string($reaction);
        if ($mechanism eq "MCMv3.2" or $mechanism eq "CRIv2") {
            unless ($reaction_string =~ /\bPAN|PPN\b/) {
                $reaction_string = "Consumption Others";
            }
        }
        my $reshape = $rate->reshape($n_per_day, $n_days);
        my $integrate = $reshape->sumover;
        $reaction_rates{$reaction_string} += $integrate->at(0);
    }
    $data{$mechanism} = \%reaction_rates;
}

my $R = Statistics::R->new();
$R->run(q` library(ggplot2) `,
        q` library(tidyr) `,
        q` library(ggthemes) `,
        q` library(dplyr) `,
        q` library(Cairo) `,
);

$R->run(q` d = data.frame() `);
foreach my $mechanism (sort keys %data) {
    $R->set('mechanism', $mechanism);
    $R->run(q` pre = data.frame(Mechanism = mechanism) `);
    foreach my $reaction (sort keys %{$data{$mechanism}}) {
        $R->set('reaction', $reaction);
        $R->set('rate', $data{$mechanism}{$reaction});
        $R->run(q` pre[reaction] = rate `);
    }
    $R->run(q` pre = pre %>% gather(Reaction, Rate, -Mechanism) `,
            q` d = rbind(d, pre) `,
    );
}

$R->run(q` net = d %>% group_by(Mechanism) %>% summarise(Net = sum(Rate)) `); 
$R->run(q` write.table(net, file = "net_RO2NO2_budget.csv", sep = ",", row.names = FALSE, quote = FALSE) `);

$R->run(q` fraction.production = d %>% filter(Rate > 0) %>% group_by(Mechanism) %>% mutate(Sum = sum(Rate), Fraction = Rate/Sum) `);
$R->run(q` write.table(fraction.production, file = "RO2NO2_production_reaction_fraction_total.csv", sep = ",", row.names = FALSE, quote = FALSE) `);

$R->run(q` fraction.consumtion = d %>% filter(Rate < 0) %>% group_by(Mechanism) %>% mutate(Sum = sum(Rate), Fraction = Rate/Sum) `);
$R->run(q` write.table(fraction.consumtion, file = "RO2NO2_consumption_reaction_fraction_total.csv", sep = ",", row.names = FALSE, quote = FALSE) `);

$R->run(q` p = ggplot(d, aes( x = Mechanism, y = Rate, fill = Reaction)) `,
        q` p = p + geom_bar(data = subset(d, Rate < 0), stat = "identity") `,
        q` p = p + geom_bar(data = subset(d, Rate > 0), stat = "identity") `,
);

$R->run(q` CairoPDF(file = "trial_RO2NO2_budgets_allocated_reactions.pdf", width = 10, height = 7) `,
        q` print(p) `,
        q` dev.off() `,
);

$R->stop();

sub get_ro2no2 {
    my ($mechanism) = @_;
    my $ro2no2_file = "${mechanism}_peroxynitrates.txt";
    open my $in, '<:encoding(utf-8)', $ro2no2_file or die $!;
    my @lines = <$in>;
    close $in;
    my @ro2no2;
    foreach my $line (@lines) {
        next if ($line =~ /^HO2NO2/);
        chomp $line;
        push @ro2no2, $line;
    }
    return \@ro2no2;
}
