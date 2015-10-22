#! /usr/bin/env perl
# plot curve of peak O3 obtained with different NOx concentrations at a set VOC level
# ARGV is the VOC_SF
# Version 0: Jane Coates 26/8/2015

use strict;
use diagnostics;
use MECCA;
use PDL;
use PDL::NiceSlice;
use Statistics::R;

die "Missing VOC Scaling Factor!" if (@ARGV == 0);
my $base = "/work/users/jco/EU_Average_Sensitivity/T_Independent_BVOCs/MOZART-4/NOx_variations-VOC_SF_" . $ARGV[0];
print "Peak O3 vs peak NOx at constant VOC SF = $ARGV[0] in $base\n";

opendir DIR, $base or die "can't open $base : $!";
my @runs = grep { $_ =~ /^NO_scaling/ } readdir DIR;
closedir DIR;

my @NOx = qw( NO NO2 );
my (@nox_conc, @o3_conc);
foreach my $run (@runs) {
    my $dir = "$base/$run";
    my $mecca = MECCA->new("$dir/boxmodel");
    #my @VOC = get_VOC($dir); 
    #$concs{"NOx"} = get_mixing_ratios($mecca, \@NOx);
    #$concs{"VOC"} = get_mixing_ratios($mecca, \@VOC);
    #$concs{"O3"} = $mecca->tracer("O3");
    my $nox_conc = get_mixing_ratios($mecca, \@NOx);
    my $o3_conc = $mecca->tracer("O3");

    push @nox_conc, $nox_conc->max * 1e9;
    push @o3_conc, $o3_conc->max * 1e9;
    #print "Max NOx = ", $concs{"NOx"}->max * 1e9, " ppb\n";
    #print "Max VOC = ", $concs{"VOC"}->max * 1e9, " ppb\n";
    #print "Max O3 = ", $concs{"O3"}->max * 1e9, " ppb\n";
}

my $R = Statistics::R->new();
$R->run(q` library(ggplot2) `,
        q` library(Cairo) `,
);

$R->set('NOx', [ @nox_conc ]);
$R->set('O3', [ @o3_conc ]);
$R->run(q` d = data.frame(NOx, O3) `);

$R->run(q` p = ggplot(d, aes(x = NOx, y = O3)) `,
        q` p = p + geom_line() `,
        q` p = p + ylab("O3 (ppbv)") + xlab("NOx (ppbv)") `
);

$R->run(q` CairoPDF(file = "O3_vs_NOx_mixing_ratios.pdf", width = 10, height = 7) `,
        q` print(p) `,
        q` dev.off() `,
);

$R->stop();

sub get_mixing_ratios {
    my ($mecca, $spc) = @_;
    my $mixing_ratio;
    foreach my $item (@$spc) {
        my $mr = $mecca->tracer($item);
        $mixing_ratio += $mr;
    }
    return $mixing_ratio;
}

sub get_VOC {
    my ($dir) = @_;
    my @VOC;
    my $emis_file = "$dir/boxmodel/MOZART_EMIS.nml";
    open my $in, '<:encoding(utf-8)', $emis_file or die $!;
    chomp (my @lines = <$in>);
    close $in;
    
    foreach my $line (@lines) {
        next unless ($line =~ /EMIS_/);
        $line =~ s/^EMIS_(.*?)=(.*?)$/$1/;
        push @VOC, $line;
    }
    return @VOC;
}
