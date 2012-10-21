# Running the program shows that we executed about
# 3,500,000 operations against our `mutex`-synchronized
# `state`.
$ go run mutexes.go
ops: 3598302
state: map[1:38 4:98 2:23 3:85 0:44]

# Next we'll look at implementing this same state
# management task using only goroutines and channels.
