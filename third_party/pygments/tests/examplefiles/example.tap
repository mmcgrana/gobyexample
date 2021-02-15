TAP version 13
1..42
1..13 A plan only supports directives so this text is wrong.
ok 1 A normal test line includes a number.
ok But a test line may also omit a number.

A random line that does not look like a test or diagnostic should be ignored.
      No matter how it is spaced out.

Or if it is a totally blank line.

not ok 3 This is a failing test line.

# Diagnostics are any lines...
# ... beginning with a hash character.

not ok 4 There are a couple of directives. # TODO is one of those directives.
not ok 5 # TODO: is invalid because the directive must be followed by a space.
ok 6 - Another directive line # toDO is not case sensitive.

ok 7 A line that is a # SKIP
ok 8 Tests can be # skipped as long as the directive has the "skip" stem.
ok 9 The TODO directive must be followed by a space, but # skip: is valid.
1..0 # Skipped directives can show on a plan line too.

Bail out! is a special phrase emitted when a TAP file aborted.

not ok 10 Having TAP version 13 in the middle of a line is not a TAP version.
not ok 11 Having Bail out! in the middle of a line is not a bail out.

ok 12 Here is an empty directive. #

# The most basic valid test lines.
ok
not ok

ok 15 Only the test number should look different. Not another 42, for example.
