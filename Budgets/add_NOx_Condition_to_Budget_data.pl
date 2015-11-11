#! /usr/bin/env perl
# add NOx- Condition to budget data. Species and date are ARGV
# Version 0: Jane Coates 11/11/2015

die "Species and/or date!" if (@ARGV < 2);
my $species = $ARGV[0];
my $date = $ARGV[1];

my @runs = qw( TI TD );
foreach my $run (@runs) {
    # first get the NOx condtions
    my $condition_file = "${run}_NOx_Conditions_each_T_NOSF.csv";
    my @lines = read_file($condition_file);

    my %data;
    foreach my $line (@lines) {
        next if ($line =~ /^Mechanism/);
        chomp $line;
        my ($mechanism, $temperature, $NOx, $condition) = split /,/, $line;
        $data{$mechanism}{$temperature}{$NOx} = $condition;
    }

    foreach my $mechanism (sort keys %data) { # add condition to the mechanism file
        foreach my $T (sort keys %{$data{$mechanism}}) {
            foreach my $NOx (sort keys %{$data{$mechanism}{$T}}) {
                #print "$T : $NOx\n";
            }
        }
        my $file = "${run}_$species/${mechanism}_${species}_budget_${date}.txt";
        my @budget = read_file($file);
        my @new_budget;
        foreach my $line (@budget) {
            chomp $line;
            if ($line =~ /^Mechanism/) {
                push @new_budget, $line . ",NOx.Condition";
            } else {
                my ($mechanism, $temperature, $NOx_mr, $NOx_emissions, $reaction, $rate, $net_rate) = split /,/, $line;
                push @new_budget, $line . ",$data{$mechanism}{$temperature}{$NOx_emissions}";
            }
        }

        #output to new file
        my $output = "${run}_$species/${mechanism}_${species}_budget_with_NOx-Condition_${date}.txt";
        open my $out, '>:encoding(utf-8)', $output or die $!;
        print $out "$_\n" foreach (@new_budget);
        close $out;
    }
}

sub read_file {
    my ($file) = @_;

    open my $in, '<:encoding(utf-8)', $file or die "Can't open $file for reading : $!";
    my @all = <$in>;
    close $in;
    return @all;
}
