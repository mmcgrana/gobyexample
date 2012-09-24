// ## Epochs

package main

import "time"

func main() {
    // Use `time.Now` with `Unix` or `UnixNane` to get
    // elapsed time since the Unix epoch.
    now := time.Now()
    secs := now.Unix()
    nanos := now.UnixNano()

    // There is no `UnixMillis`.
    millis := nanos / 1000000
    println("Secs:  ", secs)
    println("Millis:", millis)
    println("Nanos: ", nanos)
}
