// The standard library's `strings` package provides many
// useful string-related functions. 

package main

import s "strings"
import "fmt"

// Helper for all the printing we'll do in this example.
func p(o ...interface{}) {
    fmt.Println(o...)
}

func main() {
    // Here's a sampling of the functions available in
    // `strings`. Note that these are all functions from
    // package, not methods on the string object itself.
    // This means that we nned pass the string in question
    // as the first argument to the functions.
    p("Contains:  ", s.Contains("test", "es"))
    p("Count:     ", s.Count("test", "t"))
    p("HasPrefix: ", s.HasPrefix("test", "te"))
    p("HasSuffix: ", s.HasSuffix("test", "st"))
    p("Index:     ", s.Index("test", "e"))
    p("Join:      ", s.Join([]string{"a", "b"}, "-"))
    p("Repeat:    ", s.Repeat("a", 5))
    p("Replace:   ", s.Replace("foo", "o", "0", -1))
    p("Replace:   ", s.Replace("foo", "o", "0", 1))
    p("Split:     ", s.Split("a-b-c-d-e", "-"))
    p("toLower:   ", s.ToLower("TEST"))
    p("ToUpper:   ", s.ToUpper("test"))
    p()

    // You can find more functions in the [`strings`]()
    // package docs.

    // Not part of `strings` but worth mentioning here are
    // the mechanisms for getting the length of a string
    // and getting a character by index.
    p("len: ", len("hello"))
    p("char:", "hello"[1])
}
