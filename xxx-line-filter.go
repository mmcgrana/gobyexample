package main                                // A line-filter program reads input on stdin,
                                            // processes it, and prints results to stdout.
                                            // Here is an example line filter that writes a
                                            // a captialized version of all text it reads.

import "bufio"                              // Package `bufio` will help us read line-by-line.
import "bytes"
import "os"
import "io"

func main() {
    newline := []byte("\n")                 // We'll need to add our own newlines
                                            // between processed lines.

    in  := bufio.NewReader(os.Stdin)        // The buffered reader gives us `ReadLine`.
    out := os.Stdout

    for {                                   // If succesful, each `ReadLine` returns bytes and a
        inBytes, pfx, err := in.ReadLine()  // boolean indicating if don't have the whole line.
        
        if err == io.EOF {                  // The `EOF` error is expected when we reach the end
            return                          // of the input, so exit gracefully in that case.
        }                                   // Otherwise there is a problem.
        if err != nil {
            panic (err)
        }       

        outBytes := bytes.ToUpper(inBytes)  // `bytes.ToUpper` works directly on bytes, so we don't
                                            // need to convert to a string for `strings.ToUpper`.

        _, err = out.Write(outBytes)        // Write out the upercased bytes, checking for an error
        if err != nil {                     // here as well.
            panic(err)
        }

        if !pfx {                           // Unless this read was for a prefix (not the full
            out.Write(newline)              // line), we need to add our own newline.
        }        
    }
}

/*
$ cat > lines                               // Make a file with a few lowercase lines.
hello
filter
$ cat lines | go run line-filter.go         // Use the line filter to get upercase lines.
HELLO
FILTER
*/
