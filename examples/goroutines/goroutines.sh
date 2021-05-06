# When we run this program, we see the output of the
# blocking call first, then the output of the two
# goroutines. The goroutines' output may be interleaved,
# happens because goroutines are being run concurrently
# by the Go runtime.
$ go run goroutines.go
direct : 0
direct : 1
direct : 2
goroutine : 0
going
goroutine : 1
goroutine : 2
done

# Next we'll look at a complement to goroutines in
# concurrent Go programs: channels.
