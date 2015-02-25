# Running the program shows that we pick up the value
# for `FOO` that we set in the program, but that
# `BAR` is empty.
$ go run environment-variables.go
FOO: 1
BAR: 

# The list of keys in the environment will depend on your
# particular machine.
TERM_PROGRAM
PATH
SHELL
...

# If we set `BAR` in the environment first, the running
# program picks that value up.
$ BAR=2 go run environment-variables.go
FOO: 1
BAR: 2
...
