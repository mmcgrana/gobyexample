# To run the program, put the code in `hello-world.go` and
# use `go run`.
$ go run hello-world.go
Hello world

# The `go run example.go` approach is a great way to 
# experiment with Go examples, and we'll use it heavily
# throughout this book.

# Sometimes we'll need to build our sample programs
# into stand-alone binaries. We can do this using
# `go build`, which will produce a binary based on the
# name of the given Go file.
$ go build hello-world.go
$ ls
hello-world	hello-world.go

# We can then execute this binary directly.
$ ./hello-world
Hello world

# Now that we can run and build basic Go programs, let's
# learn more about the language.
