// The first timer will expire ~2s after we start the
// program, but the second should be stopped before it has
// a chance to expire.
$ go run timers.go
Timer 1 expired
Timer 2 stopped
