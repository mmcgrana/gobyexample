  | | |
"Informal by Nature"
[ * * * ]
by
[ * * * ]
David Corbett

[This is a [nested] comment.]

Section 1 - Use option translation

Use maximum tests of at least 100 translates as (-
@c
Constant MAX_TESTS = {N}; —). | Section 2

A room has a number called size.

The Kitchen is a room. "A nondescript kitchen.“ The Kitchen has size 2.

When play begins:
	say "Testing:[line break]";
	test 0.

To test (N — number): (—
	if (Test({N}) == (+size of the Kitchen [this should succeed]+)) {-open—brace}
		print ”Success.^”;
	{-close-brace} else {
		print “Failure.^";
	}
]; ! You shouldn't end a routine within a phrase definition, but it works.
[ Unused;
	#Include "\
@p \
"; ! At signs hold no power here.
! Of course, the file "@p .h" must exist.
-).

Include (-!% This is not ICL.

[ Test x;
	if (x) {x++;}
	{–! Single line comment.}
@inc x;
@p At signs.
...
@Purpose: ...
...
@-...
@c ...
@inc x;
@c
@c
	return x;
];
@Purpose: ...
@-------------------------------------------------------------------------------
-).
