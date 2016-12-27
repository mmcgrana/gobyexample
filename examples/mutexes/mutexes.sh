# Running the program shows that we executed about
# 90,000 total operations against our `mutex`-synchronized
# `state`.
$ go run mutexes.go
readOps: 83285
writeOps: 8320
state: map[1:97 4:53 0:33 2:15 3:2]

# Next we'll look at implementing this same state
# management task using only goroutines and channels.
