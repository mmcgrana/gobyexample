# When we run this program it will block waiting for a
# signal. By typing `ctrl-C` (which the
# terminal shows as `^C`) we can send a `SIGINT` signal,
# causing the program to print `interrupt` and then exit.
$ go run signals.go
awaiting signal
^C
interrupt
exiting
