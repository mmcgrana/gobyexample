package main

import (
    "os"
    "os/exec"
    "fmt"
)

func check(err error) {
    if err != nil {
        panic(err)
    }
}

func main() {
    var err error

    // Accept exactly 1 argument - the input filename.
    if len(os.Args) != 2 {
        fmt.Fprintln(os.Stderr, "Usage: tool/generate input.go > output.html")
        os.Exit(1)
    }

    // Ensure that we have `markdown` and `pygmentize` binaries.
    markdownPath, err := exec.LookPath("markdown"); 
    check(err)
    pygmentizePath, err := exec.LookPath("pygmentize")
    check(err)
    fmt.Println(markdownPath, pygmentizePath)
}
