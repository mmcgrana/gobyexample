// ## String Functions

// The standard library's `strings` package provides many
// useful string-related functions. 

package main

import "strings"
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
    p("Contains:  ", strings.Contains("test", "es"))
    p("Count:     ", strings.Count("test", "t"))
    p("HasPrefix: ", strings.HasPrefix("test", "te"))
    p("HasSuffix: ", strings.HasSuffix("test", "st"))
    p("Index:     ", strings.Index("test", "e"))
    p("Join:      ", strings.Join([]string{"a", "b"}, "-"))
    p("Repeat:    ", strings.Repeat("a", 5))
    p("Replace:   ", strings.Replace("foo", "o", "0", -1))
    p("Replace:   ", strings.Replace("foo", "o", "0", 1))
    p("Split:     ", strings.Split("a-b-c-d-e", "-"))
    p("toLower:   ", strings.ToLower("TEST"))
    p("ToUpper:   ", strings.ToUpper("test"))
    p()

    // You can find more functions in the [`strings`]()
    // package docs.

    // Not part of `strings` but worth mentioning here are
    // the mechanisms for getting the length of a string
    // and getting a character by index.
    p("len: ", len("hello"))
    p("char:", "hello"[1])
}
