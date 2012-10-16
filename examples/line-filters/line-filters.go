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
    "io"
    "log"
    "os"
    "strings"
)

func main() {

    // Wrapping the unbuffered `os.Stdin` with a buffered
    // reader gives us a convenient `ReadString` method
    // that we'll use to read input line-by-line.
    rdr := bufio.NewReader(os.Stdin)
    out := os.Stdout

    for {
        // `ReadString` returns the next string from the
        // input up to the given separator byte. We give the
        // newline byte `'\n'` as our separator so we'll get
        // successive input lines.
        switch line, err := rdr.ReadString('\n'); err {
        case nil:
            // Write out the uppercased line, checking for an
            // error on the write as we did on the read.
            ucl := strings.ToUpper(line)
            if _, err = out.WriteString(ucl); err != nil {
                log.Println(err)
                os.Exit(-1)
            }
        case io.EOF:
            // The `EOF` error is expected when we reach the
            // end of input, so exit gracefully in that case.
            os.Exit(0)
        default:
            // Otherwise there's a problem
            log.Println(err)
            os.Exit(-1)
        }
    }
}
