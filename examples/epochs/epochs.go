// A common requirement in programs is getting the number
// of seconds, milliseconds, or nanoseconds since the Unix
// epoch. Here's how to do it in Go.

package main

import "fmt"
import "time"

func main() {
    // Use `time.Now` with `Unix` or `UnixNano` to get
    // elapsed time since the Unix epoch.
    now := time.Now()
    secs := now.Unix()
    nanos := now.UnixNano()

    // Note that there is no `UnixMillis`.
    millis := nanos / 1000000
    fmt.Println("Secs:  ", secs)
    fmt.Println("Millis:", millis)
    fmt.Println("Nanos: ", nanos)
}
