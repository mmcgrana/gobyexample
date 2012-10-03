# Make a file with a few lowercase lines.
$ echo 'hello'   > /tmp/lines
$ echo 'filter' >> /tmp/lines

# Use the line filter to get uppercase lines.
$ cat /tmp/lines | go run line-filters.go
HELLO
FILTER
