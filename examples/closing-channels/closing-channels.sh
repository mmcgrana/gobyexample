$ go run closing-channels.go 
sent job 1
received job 1
sent job 2
received job 2
sent job 3
received job 3
sent all jobs
received all jobs
no jobs to receive 0

# Be sure that channel is not closed when you read
# from it, especially when iterating over a channel.
# Otherwise you might get an unexpected result or
# even enter an infinite loop.
# To learn how to correctly use `range` over channels,
# see our next example.
