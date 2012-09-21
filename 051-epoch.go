package main                    // Use `time.Now` with `Unix` or `UnixNane` to get
                                // elapsed time since the Unix epoch.
import "time"

func main() {
	now := time.Now()
	secs := now.Unix()
	nanos := now.UnixNano()
	millis := nanos / 1000000   // There is no `UnixMillis`.
	println("Secs:  ", secs)
	println("Millis:", millis)
	println("Nanos: ", nanos)
}

/*
$ go run 051-epoch.go
Secs:   1348240948
Millis: 1348240948517
Nanos:  1348240948517870000
*/
