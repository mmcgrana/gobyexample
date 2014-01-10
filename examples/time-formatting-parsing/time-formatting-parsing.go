// Go supports time formatting and parsing via
// pattern-based layouts.

package main

import "fmt"
import "time"

func main() {
    p := fmt.Println

    // Here's a basic example of formatting a time
    // according to RFC3339.
    t := time.Now()
    p(t.Format("2006-01-02T15:04:05Z07:00"))

    // `Format` uses an example-based layout approach; it
    // takes a formatted version of the reference time
    // `Mon Jan 2 15:04:05 MST 2006` to determine the
    // general pattern with which to format the given
    // time. The example time must be exactly as shown: the
    // year 2006, 15 for the hour, Monday for the day of the
    // week, etc. Here are a few more examples of time
    // formatting.
    p(t.Format("3:04PM"))
    p(t.Format("Mon Jan _2 15:04:05 2006"))
    p(t.Format("2006-01-02T15:04:05.999999-07:00"))

    // For purely numeric representations you can also
    // use standard string formatting with the extracted
    // components of the time value.
    fmt.Printf("%d-%02d-%02dT%02d:%02d:%02d-00:00\n",
        t.Year(), t.Month(), t.Day(),
        t.Hour(), t.Minute(), t.Second())

    // Time parsing uses the same example-based approach
    // as `Format`ing. These examples parse times rendered
    // with some of the layouts used above.
    withNanos := "2006-01-02T15:04:05.999999999-07:00"
    t1, e := time.Parse(
        withNanos,
        "2012-11-01T22:08:41.117442+00:00")
    p(t1)
    kitchen := "3:04PM"
    t2, e := time.Parse(kitchen, "8:41PM")
    p(t2)

    // `Parse` will return an error on malformed input
    // explaining the parsing problem.
    ansic := "Mon Jan _2 15:04:05 2006"
    _, e = time.Parse(ansic, "8:41PM")
    p(e)

    // There are several predefined formats that you can
    // use for both formatting and parsing.
    p(t.Format(time.Kitchen))
}
