# Running this program shows the first operation timing
# out and the second succeeding.
$ go run timeouts.go 
timeout 1
result 2

# Using this `select` timeout pattern requires
# communicating results over channels. This is a good
# idea in general because other important Go features are
# based on channels and `select`. We'll look at two
# examples of this next: timers and tickers.
