# Make a file with a few lowercase lines.
$ cat <<EOLINES > lines
hello
filter
EOLINES

# Use the line filter to get uppercase lines.
$ cat lines | go run line-filter.go
HELLO
WORLD
