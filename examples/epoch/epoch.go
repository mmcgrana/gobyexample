// A common requirement in programs is getting the number
// of seconds, milliseconds, or nanoseconds since the
// [Unix epoch](http://en.wikipedia.org/wiki/Unix_time).
// Here's how to do it in Go.

package main

import "fmt"
import "time"

func main() {

    // Use `time.Now` with `Unix` or `UnixNano` to get
    // elapsed time since the Unix epoch in seconds or
    // nanoseconds, respectively.
    now := time.Now()
    secs := now.Unix()
    nanos := now.UnixNano()

    // Note that there is no `UnixMillis`, so to get the
    // milliseconds since epoch you'll need to manually
    // dive from nanoseconds.
    millis := nanos / 1000000
    fmt.Println("secs:  ", secs)
    fmt.Println("millis:", millis)
    fmt.Println("nanos: ", nanos)
}
