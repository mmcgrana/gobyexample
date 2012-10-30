// Go offers built-in support for [regular expressions](http://en.wikipedia.org/wiki/Regular_expression).
// Here are some examples of  common regexp-related tasks
// in Go.

package main

import "fmt"
import "regexp"

func main() {

    // This tests whether a pattern matches a string.
    match, _ := regexp.MatchString("p[a-z]+ch", "apple")
    fmt.Println("match:", match)

    // In the above example we used a string pattern
    // directly. For other regular expression tasks you'll
    // need to `Compile` a `Regexp` struct.
    r1, _ := regexp.Compile("p[a-z]+ch")

    // Many methods are available on these structs. Here's
    // a match test like we saw earlier.
    fmt.Println("match:", r1.MatchString("apple"))

    //

    // When creating top-level constants with regular
    // expressions, you can use the `MustCompile` variant
    // of the `Compile` function we saw earlier. A plain
    // `Compile` won't work for constants because it has 2
    // return values.
    cr := regexp.MustCompile("p[a-z]+ch")
    fmt.Println("regex:", cr)

}

// todo: 
// todo: gsub

// todo: Examples of regular expressions in #golang: https://gobyexample.com/regular-expressions One of the best areas for a "by example" approach IMO.
