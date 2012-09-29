// ## Line Filters

// Generate literate-programming style HTML
// documentation form Go source files.

package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "os/exec"
    "regexp"
    "strings"
)

// Recognize doc lines, extract their comment prefixes.
var docsPat = regexp.MustCompile("^\\s*\\/\\/\\s")

// Abort on non-nil errors.
func check(err error) {
    if err != nil {
        panic(err)
    }
}

// For docs and code rendering: pipe source data
// through binary at path with given argv, return
// the output.
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

// We'll break the code into {docs, code} pairs,
// and then render those text segments before
// including them in the HTML doc.
type segment struct {
    docs, code, docsRendered, codeRendered string
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

    // Read the source file in, split into lines.
    sourceBytes, err := ioutil.ReadFile(os.Args[1])
    check(err)
    lines := strings.Split(string(sourceBytes), "\n")

    // Group lines into docs/code segments.
    // Special case the header to go in its own segment.
    segments := []*segment{}
    segments = append(segments, &segment{code: "", docs: docsPat.ReplaceAllString(lines[0], "")})
    segments = append(segments, &segment{code: "", docs: ""})
    lastLine := ""
    for _, line := range lines[2:] {
        head := segments[len(segments)-1]
        // Doc line - trim off the comment markers.
        if (line == "" && lastLine == "docs") || docsPat.MatchString(line) {
            trimLine := docsPat.ReplaceAllString(line, "")
            if !(lastLine == "code" && head.docs != "") {
                head.docs = head.docs + "\n" + trimLine
            } else {
                segments = append(segments, &segment{docs: trimLine, code: ""})
            }
            lastLine = "docs"
            // Code line - preserve all whitespace.
        } else {
            if !(lastLine == "docs" && head.code != "") {
                head.code = head.code + "\n" + line
            } else {
                segments = append(segments, &segment{docs: "", code: line})
            }
            lastLine = "code"
        }
    }

    // Render docs via `markdown` and code via
    // `pygmentize` in each segment.
    for _, seg := range segments {
        seg.docsRendered = pipedCmd(markdownPath, []string{}, seg.docs)
        seg.codeRendered = pipedCmd(pygmentizePath, []string{"-l", "go", "-f", "html"}, seg.code+"  ")
    }

    // Print HTML header.
    fmt.Print(`
<!DOCTYPE html>
<html>
  <head>
    <meta http-eqiv="content-type" content="text/html;charset=utf-8">
    <title>Page Title</title>
    <link rel=stylesheet href="book.css">
  </head>
  <body>
    <div id="container">
      <div id="background"></div>
      <table cellspacing="0" cellpadding="0">
        <thead>
          <tr><td class=docs></td><td class=code></td></tr>
        </thead>
        <tbody>`)

    // Print HTML docs/code segments.
    for _, seg := range segments {
        fmt.Printf("<tr><td class=docs>%s</td><td class=code>%s</td></tr>\n", seg.docsRendered, seg.codeRendered)
    }

    // Print HTML footer.
    fmt.Print(`
        </tbody>
      </table>
    </div>
  </body>
</html>
`)
}
