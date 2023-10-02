# We expect to get exactly 50,000 operations. Had we
# used a non-atomic integer and incremented it with
# `ops++`, we'd likely get a different number,
# changing between runs, because the goroutines
# would interfere with each other. Moreover, we'd
# get data race failures when running with the
# `-race` flag.
$ go run atomic-counters.go
ops: 50000

# Next we'll look at mutexes, another tool for managing
# state.
