# Make a file with a few lowercase lines.
$ cat > lines                               
hello
filter

# Use the line filter to get uppercase lines.
$ cat lines | go run line-filter.go
HELLO
FILTER
