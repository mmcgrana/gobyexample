// A _line filter_ is a common type of program that reads
// input on stdin, processes it, and then prints some
// derived result to stdout. `grep` and `sed` are common
// line filters.

// Here's an example line filter in Go that writes a
// capitalized version of all input text. You can use this
// pattern to write your own Go line filters.
package main

import (
    "bufio"
    "fmt"
    "io"
    "os"
    "strings"
)

func main() {

    // Wrapping the unbuffered `os.Stdin` with a buffered
    // scanner gives us a convenient `Scan` method that
    // advances the scanner to the next token; next line by
    // default.
    scanner := bufio.NewScanner(os.Stdin)
    out := os.Stdout

    // `Text` returns the current token, next line from the
    // input.
    for scanner.Scan() {
        ucl := strings.ToUpper(scanner.Text())
        if _, err = out.WriteString(ucl); err != nil {
            fmt.Fprintln(os.Stderr, "error:", err)
            os.Exit(1)
        }
    }
    
    // Check for errors during `Scan`. End of file is
    // expected and not reported as an error.
    if err := scanner.Err(); err != nil {
        fmt.Fprintln(os.Stderr, "error:", err)
        os.Exit(1)
    } else {
        os.Exit(0)
    }
}
