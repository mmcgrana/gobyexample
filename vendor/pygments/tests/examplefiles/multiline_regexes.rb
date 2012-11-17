/
this is a
multiline
regex
/

this /is a
multiline regex too/

foo = /is also
one/

also /4
is one/

this(/
too
/)

# this not
2 /4
asfsadf/

# this is also not one
0x4d /25
foo/

42 and /this
is also a multiline
regex/


# And here some special string cases
foo = % blah            # comment here to ensure whitespace
foo(% blah )
foo << % blah           # stupid but has to work
foo = % blah + % blub   # wicked
foo = %q wicked         # works too
