// Go supports time formatting and parsing via
// pattern-based layouts.

package main

import "fmt"
import "time"

func check(e error) {
	if e != nil {
		panic(e)
	}
}
func main() {
	p := fmt.Println

	// Here's a basic example of formatting a time
	// according to RFC3339, using the corresponding layout
	// constant.
	t := time.Now()
	p(t.Format(time.RFC3339))

	// Time parsing uses the same layout values as `Format`.
	t1, e := time.Parse(
		time.RFC3339,
		"2012-11-01T22:08:41+00:00")
	check(e)
	p(t1)

	// `Format` and `Parse` uses example-based layouts. Usually
	// you'll use a constant from `time` for these layouts, but
	// you can also supply custom layouts. Layouts must use the
	// reference time `Mon Jan 2 15:04:05 MST 2006` to show the
	// pattern with which to format/parse a given time/string.
	// The example time must be exactly as shown: the year 2006,
	// 15 for the hour, Monday for the day of the week, etc.
	p(t.Format("3:04PM"))
	p(t.Format("Mon Jan _2 15:04:05 2006"))
	p(t.Format("2006-01-02T15:04:05.999999-07:00"))
	form := "3 04 PM"
	t2, e := time.Parse(form, "8 41 PM")
	check(e)
	p("Parse:", t2.Format(time.RFC3339))

	// Generally, we want to save time in Local time zone.
	loc, e := time.LoadLocation("Local")
	check(e)
	t3, e := time.ParseInLocation(form, "8 41 PM", loc)
	check(e)
	p("ParseInLocation:", t3.Format(time.RFC3339))

	// For purely numeric representations you can also
	// use standard string formatting with the extracted
	// components of the time value.
	fmt.Printf("%d-%02d-%02dT%02d:%02d:%02d-00:00\n",
		t.Year(), t.Month(), t.Day(),
		t.Hour(), t.Minute(), t.Second())

	// `Parse` will return an error on malformed input
	// explaining the parsing problem.
	yyyymm := "2006"
	_, e = time.Parse(yyyymm, "08:41")
	p(e)
}
