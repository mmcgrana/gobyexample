# Try running the file-writing code.
$ go run writing-files.go 
wrote 5 bytes
wrote 7 bytes
wrote 9 bytes

# Then check the contents of the written files.
$ cat /tmp/dat1
hello
go
$ cat /tmp/dat2
some
writes
buffered

# Next we'll look at applying some of the file I/O ideas
# we've just seen to the `stdin` and `stdout` streams.
