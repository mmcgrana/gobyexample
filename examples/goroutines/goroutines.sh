# When we run this program, we see the output of the
# blocking call first, then the interleaved output of the
# two goroutines. This interleaving reflects the
# goroutines being run concurrently by the Go runtime.
$ go run goroutines.go
direct : 0
direct : 1
direct : 2
goroutine : 0
going
goroutine : 1
goroutine : 2
<enter>
done

# Next we'll look at a complement to goroutines in
# concurrent Go programs: channels.
