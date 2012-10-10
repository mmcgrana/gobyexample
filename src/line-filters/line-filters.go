// A _line filter_ is a common type of program that reads
// input on stdin, processes it, and then prints some
// derived result to stdout. `grep` and `sed` are common
// line filters.

// Here's an example line filter in Go that writes a
// capitalized version of all input text. You can use this
// pattern to write your own Go line filters.
package main

// Package `bufio` will help us read line-by-line, and
// `bytes` provides the byte-level capitaliazation
// function.
import "bufio"
import "bytes"
import "os"
import "io"

func main() {

    // Wrapping the unbuffered `os.Stdin` with a buffered
    // reader gives us the convenient `ReadLine` method.
    in := bufio.NewReader(os.Stdin)
    out := os.Stdout

    // Each `ReadLine` call returns bytes of read data and
    // a boolean indicating if we don't have the whole
    // line yet, or an error.
    for {
        inBytes, pfx, err := in.ReadLine()

        // The `EOF` error is expected when we reach the
        // end of input, so exit gracefully in that case.
        // Otherwise there's a problem.
        if err == io.EOF {
            return
        }
        if err != nil {
            panic(err)
        }

        // Write out the uppercased bytes, checking for an
        // error here as well.
        outBytes := bytes.ToUpper(inBytes)
        _, err = out.Write(outBytes)
        if err != nil {
            panic(err)
        }

        // Unless this read was for a prefix (not the full
        // line), we need to add our own newline.
        if !pfx {
            out.WriteString("\n")
        }
    }
}
