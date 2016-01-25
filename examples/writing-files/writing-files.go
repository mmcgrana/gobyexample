// Writing files in Go follows similar patterns to the
// ones we saw earlier for reading.

package main

import (
    "bufio"
    "fmt"
    "io/ioutil"
    "os"
)

// Printing an error and then exiting is an appropriate way
// to handle errors at the top level of a command line
// program, but not in general purpose library functions.
func check(err error) {
    if err != nil {
        fmt.Println("Fatal:", err)
        os.Exit(1)
    }
}

func main() {

    // To start, here's how to dump a string (or just
    // bytes) into a file.
    d1 := []byte("hello\ngo\n")
    err := ioutil.WriteFile("/tmp/dat1", d1, 0644)
    check(err)

    // For more granular writes, open a file for writing.
    f, err := os.Create("/tmp/dat2")
    check(err)

    // You can `Write` byte slices as you'd expect.
    d2 := []byte{115, 111, 109, 101, 10}
    n2, err := f.Write(d2)
    check(err)
    fmt.Printf("wrote %d bytes\n", n2)

    // A `WriteString` is also available.
    n3, err := f.WriteString("writes\n")
    fmt.Printf("wrote %d bytes\n", n3)

    // Issue a `Sync` to flush writes to stable storage.
    // This is the point where many errors show up, as it
    // may be the first time the operating system actually
    // touches the disk. Don't forget to check for errors.
    err = f.Sync()
    check(err)

    // `bufio` provides buffered writers in addition
    // to the buffered readers we saw earlier.
    w := bufio.NewWriter(f)
    n4, err := w.WriteString("buffered\n")
    check(err)
    fmt.Printf("wrote %d bytes\n", n4)

    // Use `Flush` to ensure all buffered operations have
    // been applied to the underlying writer. Same thing
    // about errors applies here as for Sync() above.
    err = w.Flush()
    check(err)

    // At the end, we need to close the file and, again,
    // check for any write errors.
    err = f.Close()
    check(err)

}
