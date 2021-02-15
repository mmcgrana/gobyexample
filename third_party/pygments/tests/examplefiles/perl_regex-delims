#! /usr/bin/env perl

use strict;
use warnings;

# common delimiters
print "a: ";
my $a = "foo";
print $a, " - ";
$a =~ s/foo/bar/;
print $a, "\n";

print "b: ";
my $b = "foo";
print $b, " - ";
$b =~ s!foo!bar!;
print $b, "\n";

print "c: ";
my $c = "foo";
print $c, " - ";
$c =~ s@foo@bar@;
print $c, "\n";

print "d: ";
my $d = "foo";
print $d, " - ";
$d =~ s\foo\bar\;
print $d, "\n";

print "\n";

# balanced delimiters
print "e: ";
my $e = "foo";
print $e, " - ";
$e =~ s{foo}{bar};
print $e, "\n";

print "f: ";
my $f = "foo";
print $f, " - ";
$f =~ s(foo)(bar);
print $f, "\n";

print "g: ";
my $g = "foo";
print $g, " - ";
$g =~ s<foo><bar>;
print $g, "\n";

print "h: ";
my $h = "foo";
print $h, " - ";
$h =~ s[foo][bar];
print $h, "\n";

print "\n";

# balanced delimiters with whitespace
print "i: ";
my $i = "foo";
print $i, " - ";
$i =~ s{foo} {bar};
print $i, "\n";

print "j: ";
my $j = "foo";
print $j, " - ";
$j =~ s<foo>		<bar>;
print $j, "\n";

print "k: ";
my $k = "foo";
print $k, " - ";
$k =~
	s(foo)

	(bar);
print $k, "\n";

print "\n";

# mixed delimiters
print "l: ";
my $l = "foo";
print $l, " - ";
$l =~ s{foo} <bar>;
print $l, "\n";

print "m: ";
my $m = "foo";
print $m, " - ";
$m =~ s(foo) !bar!;
print $m, "\n";

print "n: ";
my $n = "foo";
print $n, " - ";
$n =~ s[foo] $bar$;
print $n, "\n";

print "\n";

# /x modifier
print "o: ";
my $o = "foo";
print $o, " - ";
$o =~ s{
				foo
			 } {bar}x;
print $o, "\n";

print "p: ";
my $p = "foo";
print $p, " - ";
$p =~ s%
  foo
  %bar%x;
print $p, "\n";
