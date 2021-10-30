# Running the program shows that the counters
# updated as expected.
$ go run mutexes.go
map[a:20000 b:10000]

# Next we'll look at implementing this same state
# management task using only goroutines and channels.
