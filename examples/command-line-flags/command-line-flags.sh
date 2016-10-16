# To experiment with the command-line flags program it's
# best to first compile it and then run the resulting
# binary directly.
$ go build command-line-flags.go

# Try out the built program by first giving it values for
# all flags.
$ ./command-line-flags -word=opt -numb=7 -fork -svar=flag
word: opt
numb: 7
fork: true
svar: flag
tail: []

# Note that if you omit flags they automatically take
# their default values.
$ ./command-line-flags -word=opt
word: opt
numb: 42
fork: false
svar: bar
tail: []

# Trailing positional arguments can be provided after
# any flags.
$ ./command-line-flags -word=opt a1 a2 a3
word: opt
...
tail: [a1 a2 a3]

# Note that the `flag` package requires all flags to
# appear before positional arguments (otherwise the flags
# will be interpreted as positional arguments).
$ ./command-line-flags -word=opt a1 a2 a3 -numb=7
word: opt
numb: 42
fork: false
svar: bar
tail: [a1 a2 a3 -numb=7]

# Use `-h` or `--help` flags to get automatically
# generated help text for the command-line program.
$ ./command-line-flags -h
Usage of ./command-line-flags:
  -fork=false: a bool
  -numb=42: an int
  -svar="bar": a string var
  -word="foo": a string

# If you provide a flag that wasn't specified to the
# `flag` package, the program will print an error message
# and show the help text again.
$ ./command-line-flags -wat
flag provided but not defined: -wat
Usage of ./command-line-flags:
...

# Next we'll look at environment variables, another common
# way to parameterize programs.
