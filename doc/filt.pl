#!C:/Perl/bin/perl.exe -w
# This script creates an HTML file from a Prolog program.
my $outfile = $ARGV[0];

$outfile =~ s/\.pl$/.html/;

open IN,"<$ARGV[0]";
open OUT,">$outfile";
while(<IN>)
  {
    s/^%//g;
    s/<code>/<font size=4><b><pre>/g;
    s/<\/code>/<\/pre><\/b><\/font>/g;
    print OUT $_;
  }
close IN;
close OUT;
