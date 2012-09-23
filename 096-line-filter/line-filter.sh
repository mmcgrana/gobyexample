# Make a file with a few lowercase lines.
$ echo 'hello'   > lines
$ echo 'filter' >> lines

# Use the line filter to get uppercase lines.
$ cat lines | go run line-filter.go
HELLO
FILTER
