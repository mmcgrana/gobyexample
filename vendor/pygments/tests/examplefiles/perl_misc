#!/usr/bin/perl

# from http://gist.github.com/485595
use strict;
use warnings;
use Time::HiRes 'usleep';

for (1..5) {
    open my $in, '<', '/proc/sys/kernel/random/entropy_avail' or die;
    print <$in>;
    close $in;
    usleep 100_000;
}

# other miscellaneous tests of numbers separated by _
#usleep 100_000;
100_000_000;
my $nichts = 0.005_006;
print "$nichts\n";
my $nichts2 = 0.005_006_007;
print 900_800_700.005_006_007, $/;

# numbers from `man 1 perlnumber`
my $n;
$n = 1234;              # decimal integer
$n = 0b1110011;         # binary integer
$n = 01234;             # octal integer
$n = 0x1234;            # hexadecimal integer
$n = 12.34e-56;         # exponential notation
$n = "-12.34e56";       # number specified as a string
$n = "1234";            # number specified as a string

# other numbers
for (
    -9876,
    +8765,
    -9876.02,
    -9876.02e+10,
    +765_432e30,
    2002.,
    .2002,
) {
    print $_, "\n";
}

# operators on numbers
for (
    $n + 300,
    $n - 300,
    $n / 300 + 10,
    $n * 250 / 2.0,
    $n == 100,
    $n != 100,
    $n > 100,
    $n >= 100,
    $n < 100,
    $n <= 100,
    $n % 2,
    abs $n,
) {
    print $_, "\n";
}
