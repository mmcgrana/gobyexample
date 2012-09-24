// Generate HTTML document form a Go source file.

package main

import (
    "os"
    "os/exec"
    "io/ioutil"
    "fmt"
)

// Just be sure to blow up on non-nil errors.
func check(err error) {
    if err != nil {
        panic(err)
    }
}

func pipedCmd(path string, argv []string, source string) string {
    cmd := exec.Command(path, argv...)
    in, err := cmd.StdinPipe()
    check(err)
    out, err := cmd.StdoutPipe()
    check(err)
    err = cmd.Start()
    check(err)
    in.Write([]byte(source))
    check(err)
    err = in.Close()
    check(err)
    bytes, err := ioutil.ReadAll(out)
    check(err)
    err = cmd.Wait()
    check(err)
    return string(bytes)
}

func main() {
    // Accept exactly 1 argument - the input filename.
    if len(os.Args) != 2 {
        fmt.Fprintln(os.Stderr, "Usage: tool/generate input.go > output.html")
        os.Exit(1)
    }

    // Ensure that we have `markdown` and `pygmentize`,
    // binaries, remember their paths.
    markdownPath, err := exec.LookPath("markdown"); 
    check(err)
    pygmentizePath, err := exec.LookPath("pygmentize")
    check(err)

    fmt.Print(pipedCmd(markdownPath, []string{}, "## wat"))

    fmt.Print(pipedCmd(pygmentizePath, []string{"-l", "go", "-f", "html"}, "package main"))
}
