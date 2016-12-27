// Go offers extensive support for times and durations;
// here are some examples.

package main

import "fmt"
import "time"

func main() {
    p := fmt.Println

    // We'll start by getting the current time.
    now := time.Now()
    p(now)

    // You can build a `time` struct by providing the
    // year, month, day, etc. Times are always associated
    // with a `Location`, i.e. time zone.
    then := time.Date(
        2009, 11, 17, 20, 34, 58, 651387237, time.UTC)
    p(then)

    // You can extract the various components of the time
    // value as expected.
    p(then.Year())
    p(then.Month())
    p(then.Day())
    p(then.Hour())
    p(then.Minute())
    p(then.Second())
    p(then.Nanosecond())
    p(then.Location())

    // The Monday-Sunday `Weekday` is also available.
    p(then.Weekday())

    // These methods compare two times, testing if the
    // first occurs before, after, or at the same time
    // as the second, respectively.
    p(then.Before(now))
    p(then.After(now))
    p(then.Equal(now))

    // The `Sub` methods returns a `Duration` representing
    // the interval between two times.
    diff := now.Sub(then)
    p(diff)

    // We can compute the length of the duration in
    // various units.
    p(diff.Hours())
    p(diff.Minutes())
    p(diff.Seconds())
    p(diff.Nanoseconds())

    // You can use `Add` to advance a time by a given
    // duration, or with a `-` to move backwards by a
    // duration.
    p(then.Add(diff))
    p(then.Add(-diff))
}
