// Generate HTTML document form a Go source file.

package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "os/exec"
    "regexp"
    "strings"
)

var docsPat = regexp.MustCompile("^\\s*\\/\\/\\s")

// Just be sure to blow up on non-nil errors.
func check(err error) {
    if err != nil {
        panic(err)
    }
}

// Pipe source data through binary at path with
// given argv, return the output.
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

type segment struct {
    docs, code string
}

func main() {
    // Accept exactly 1 argument - the input filename.
    if len(os.Args) != 2 {
        fmt.Fprintln(os.Stderr, "Usage: tool/generate input.go > output.html")
        os.Exit(1)
    }

    // Ensure that we have `markdown` and `pygmentize`,
    // binaries, remember their paths.
    markdownPath, err := exec.LookPath("markdown")
    check(err)
    pygmentizePath, err := exec.LookPath("pygmentize")
    check(err)
    fmt.Println(markdownPath, pygmentizePath)

    // Read the source file in.
    sourceBytes, err := ioutil.ReadFile(os.Args[1])
    check(err)

    // Split into lines.
    lines := strings.Split(string(sourceBytes), "\n")

    // Group lines into docs/code segments.
    segments := []*segment{}
    segments = append(segments, &segment{code: "", docs: ""})
    lastLine := ""
    for _, line := range lines {
        head := segments[len(segments)-1]
        // Doc line. Trim off the comment markers.
        if (line == "" && lastLine == "docs") || docsPat.MatchString(line) {
            trimLine := docsPat.ReplaceAllString(line, "")
            if !(lastLine == "code" && head.docs != "") {
                head.docs = head.docs + "\n" + trimLine
            } else {
                segments = append(segments, &segment{docs: trimLine, code: ""})
            }
            lastLine = "docs"
            // Code line. Preserve all whitespace.
        } else {
            if !(lastLine == "docs" && head.code != "") {
                head.code = head.code + "\n" + line
            } else {
                segments = append(segments, &segment{docs: "", code: line})
            }
            lastLine = "code"
        }
    }
    for _, seg := range segments {
        fmt.Printf("%#v\n", *seg)
    }
}
