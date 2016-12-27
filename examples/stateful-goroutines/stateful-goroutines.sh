# Running our program shows that the goroutine-based
# state management example completes about 80,000
# total operations.
$ go run stateful-goroutines.go
readOps: 71708
writeOps: 7177

# For this particular case the goroutine-based approach
# was a bit more involved than the mutex-based one. It
# might be useful in certain cases though, for example
# where you have other channels involved or when managing
# multiple such mutexes would be error-prone. You should
# use whichever approach feels most natural, especially
# with respect to understanding the correctness of your
# program.
