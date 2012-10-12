// A _line filter_ is a common type of program that reads
// input on stdin, processes it, and then prints some
// derived result to stdout. `grep` and `sed` are common
// line filters.

// Here's an example line filter in Go that writes a
// capitalized version of all input text. You can use this
// pattern to write your own Go line filters.
package main

// Package `bufio` will help us read line-by-line.
import "bufio"
import "strings"
import "os"
import "io"

func main() {

    // Wrapping the unbuffered `os.Stdin` with a buffered
    // reader gives us a convenient `ReadString` method
    // that we'll use to read input line-by-line.
    in := bufio.NewReader(os.Stdin)
    out := os.Stdout

    // `ReadString` returns the next string from the
    // input up to the given separator byte. We give the
    // newline byte `'\n'` as our separator so we'll get
    // successive input lines.
    for {
        inLine, err := in.ReadString('\n')

        // The `EOF` error is expected when we reach the
        // end of input, so exit gracefully in that case.
        // Otherwise there's a problem.
        if err == io.EOF {
            return
        }
        if err != nil {
            panic(err)
        }

        // Write out the uppercased line, checking for an
        // error on the write as we did on the read.
        outLine := strings.ToUpper(inLine)
        _, err = out.WriteString(outLine)
        if err != nil {
            panic(err)
        }
    }
}
