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
    // reader gives us a convenient `ReadString` method
    // that we'll use to read input line-by-line.
    rdr := bufio.NewReader(os.Stdin)
    out := os.Stdout

    // `ReadString` returns the next string from the
    // input up to the given separator byte. We give the
    // newline byte `'\n'` as our separator so we'll get
    // successive input lines.
    for {
        switch line, err := rdr.ReadString('\n'); err {

        // If the read succeeded (the read `err` is nil),
        // write out out the uppercased line. Check for an
        // error on this write as we do on the read.
        case nil:
            ucl := strings.ToUpper(line)
            if _, err = out.WriteString(ucl); err != nil {
                fmt.Fprintln(os.Stderr, "error:", err)
                os.Exit(1)
            }

        // The `EOF` error is expected when we reach the
        // end of input, so exit gracefully in that case.
        case io.EOF:
            os.Exit(0)

        // Otherwise there's a problem; print the
        // error and exit with non-zero status.
        default:
            fmt.Fprintln(os.Stderr, "error:", err)
            os.Exit(1)
        }
    }
}
