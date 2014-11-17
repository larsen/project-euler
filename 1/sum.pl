use strict;
use warnings;

my $sum = 0;
foreach my $i ( 1 .. 999 ) {
    if ( (not ($i % 3)) || (not ($i % 5)) ) {
        print "Adding $i\n";
        $sum += $i;
    }
}

print "Sum: $sum\n";
