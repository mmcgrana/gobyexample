// A common requirement in programms is getting the number
// of seconds, milliseconds, or nanoseconds since the Unix
// epoch. Here's how to do it in Go.

package main

import "time"

func main() {
    // Use `time.Now` with `Unix` or `UnixNano` to get
    // elapsed time since the Unix epoch.
    now := time.Now()
    secs := now.Unix()
    nanos := now.UnixNano()

    // Note that there is no `UnixMillis`.
    millis := nanos / 1000000
    println("Secs:  ", secs)
    println("Millis:", millis)
    println("Nanos: ", nanos)
}
