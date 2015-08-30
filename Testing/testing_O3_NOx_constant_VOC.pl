#! /usr/bin/env perl
# extract NOx, VOC and O3 data from check file, for each mechanism at VOC SF = 2.1, 2.5, 3.0 and plot O3 as function of NOx. Date (ddmmyyyy) is argument
# Version 0: Jane Coates 28/8/2015

use strict;
use diagnostics;
use Statistics::R;

die "No date of check file" if (@ARGV == 0);
my $checkfile = "check_" . $ARGV[0] . ".txt";
print "Extracting data from $checkfile\n";

open my $in, '<:encoding(utf-8)', $checkfile or die $!;
local $/ = undef; #slurp mode
my $all_lines = <$in>;
close $in;

my %data;
my ($sf_2_1) = $all_lines =~ /VOC_SF_2\.1(.*?)VOC_SF_2\.2/s;
($data{"VOC_SF_2.1"}{"NOx"}, $data{"VOC_SF_2_1"}{"O3"}) = extract_data($sf_2_1);

my $R = Statistics::R->new();
$R->run(q` library(ggplot2) `,
        q` library(Cairo) `,
        q` library(tidyr) `,
);
$R->run(q` d = data.frame() `);
#print scalar @{$data{"VOC_SF_2_1"}{"O3"}}, "\n";
$R->set('rep.nr', scalar @{$data{"VOC_SF_2_1"}{"O3"}});
foreach my $run (sort keys %data) {
    $R->set('run', $run);
    $R->run(q` pre = data.frame(Run = rep(run, rep.nr)) `);
    foreach my $spc (sort keys %{$data{$run}}) {
        $R->set('spc', $spc);
        $R->set('mr', [@{$data{$run}{$spc}}]);
        $R->run(q` pre[spc] = mr `);
    }
    $R->run(q` pre = gather(pre, Spc, Mixing.Ratio, -Run) `,
            q` d = rbind(d, pre) `,
    );
}
my $p = $R->run(q` print(d) `);
print $p, "\n";

$R->stop();

sub extract_data {
    my ($lines) = @_;
    my @lines = split /\n/, $lines;
    my (@nox, @o3);
    foreach my $line (@lines) {
         next unless ($line =~ /Max/);
         if ($line =~ /NOx/) {
             push @nox, get_value($line);
         } elsif ($line =~ /O3/) {
             push @o3, get_value($line);
         }
    }
    return \@nox, \@o3;
}

sub get_value {
    my ($line) = @_;

    my ($item, $value) = split / : /, $line;
    my ($number, $unit) = split / /, $value;
    my $return = sprintf "%.5f", $number;
    return $return;
}
