// ## String Functions

package main

import "strings"
import "fm"

func p(o interface{}) {
	fmt.Println(o)
}

func main() {
    p("hello"[0])
    p(strings.Contains("test", "es"))
    p(strings.Count("test", "t"))
    p(strings.HasPrefix("test", "te"))
    p(strings.HasSuffix("test", "st"))
	p(strings.Index("test", "e"))
    p(strings.Join([]string{"a","b"}, "-"))
    p(strings.Repeat("a", 5))
    p(strings.Replace("foo", "o", "0", -1))
    p(strings.Replace("foo", "o", "0", 1))
    p(strings.Split("a-b-c-d-e", "-"))
    p(strings.ToLower("TEST"))
    p(strings.ToUpper("test"))
}
