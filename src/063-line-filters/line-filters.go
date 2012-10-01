// ## Line Filters

// A _line filter_ program reads input on stdin,
// processes it, and prints results to stdout.
// Here's an example line filter that writes
// a capitalized version of all text it reads.
package main

// Package `bufio` will help us read line-by-line.
import "bufio"
import "bytes"
import "os"
import "io"

// We'll need to add our own newlines between
// processed lines.
func main() {
    newline := []byte("\n")

    // The buffered reader gives us `ReadLine`.
    in := bufio.NewReader(os.Stdin)
    out := os.Stdout

    // If successful, each `ReadLine` returns bytes and a
    // boolean indicating if don't have the whole line yet.
    for {
        inBytes, pfx, err := in.ReadLine()

        // The `EOF` error is expected when we reach the
        // end of the input, so exit gracefully in that
        // case. Otherwise there is a problem.
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
            out.Write(newline)
        }
    }
}
