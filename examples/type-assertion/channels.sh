# When we run the program the `"ping"` message is
# successfully passed from one goroutine to another via
# our channel.
$ go run channels.go 
ping

# By default sends and receives block until both the
# sender and receiver are ready. This property allowed
# us to wait at the end of our program for the `"ping"`
# message without having to use any other synchronization.
