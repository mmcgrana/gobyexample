// ## golit

// **golit** generates literate-programming style HTML
// documentation from a Go source files.
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

// Recognize title prefixes, for titling web page.
var titlePat = regexp.MustCompile("^\\/\\/\\s##\\s")

// Abort on non-nil errors.
func check(err error) {
    if err != nil {
        panic(err)
    }
}

// We'll implement Markdown rendering and Pygments
// syntax highlighting by piping the source data through
// external programs. This is a general helper for
// handling both cases.
func pipe(bin string, arg []string, src string) string {
    cmd := exec.Command(bin, arg...)
    in, err := cmd.StdinPipe()
    check(err)
    out, err := cmd.StdoutPipe()
    check(err)
    err = cmd.Start()
    check(err)
    in.Write([]byte(src))
    check(err)
    err = in.Close()
    check(err)
    bytes, err := ioutil.ReadAll(out)
    check(err)
    err = cmd.Wait()
    check(err)
    return string(bytes)
}

// We'll break the code into {docs, code} pairs, and
// then render those text segments before including them
// in the HTML doc.
type seg struct {
    docs, code, docsRendered, codeRendered string
}

func main() {
    // Accept exactly 1 argument - the input filename,
    // less the .go extension.
    if len(os.Args) != 2 {
        fmt.Fprintln(os.Stderr, "unexpected args")
        os.Exit(1)
    }

    // Ensure that we have `markdown` and `pygmentize`,
    // binaries, remember their paths.
    markdownPath, err := exec.LookPath("markdown")
    check(err)
    pygmentizePath, err := exec.LookPath("pygmentize")
    check(err)

    // Read the source file in, split into lines.
    srcBytes, err := ioutil.ReadFile(os.Args[1]+".go")
    check(err)
    lines := strings.Split(string(srcBytes), "\n")

    // Group lines into docs/code segments. First,
    // special case the header to go in its own segment.
    headerDoc := docsPat.ReplaceAllString(lines[0], "")
    segs := []*seg{}
    segs = append(segs, &seg{code: "", docs: headerDoc})

    // Then handle the remaining as code/doc pairs.
    segs = append(segs, &seg{code: "", docs: ""})
    last := ""
    for _, line := range lines[2:] {
        head := segs[len(segs)-1]
        docMatch := docsPat.MatchString(line)
        docLast := last == "docs"
        codeLast := last == "code"
        if docMatch || (line == "" && docLast) {
            trimed := docsPat.ReplaceAllString(line, "")
            if !(last == "code" && head.docs != "") {
                head.docs = head.docs + "\n" + trimed
            } else {
                newSeg := seg{docs: trimed, code: ""}
                segs = append(segs, &newSeg)
            }
            last = "docs"
            // Code line - preserve all whitespace.
        } else {
            if !(codeLast && head.code != "") {
                head.code = head.code + "\n" + line
            } else {
                newSeg := seg{docs: "", code: line}
                segs = append(segs, &newSeg)
            }
            last = "code"
        }
    }

    // Render docs via `markdown` and code via
    // `pygmentize` in each segment.
    for _, seg := range segs {
        seg.docsRendered = pipe(
            markdownPath,
            []string{},
            seg.docs)
        seg.codeRendered = pipe(
            pygmentizePath,
            []string{"-l", "go", "-f", "html"},
            seg.code+"  ")
    }

    // Print HTML header.
    title := titlePat.ReplaceAllString(lines[0], "")
    fmt.Printf(`
<!DOCTYPE html>
<html>
  <head>
    <meta http-eqiv="content-type"
          content="text/html;charset=utf-8">
    <title>%s</title>
    <link rel=stylesheet href="../style/lit.css">
  </head>
  <body>
    <div id="container">
      <div id="background"></div>
      <table cellspacing="0" cellpadding="0">
        <thead>
          <tr>
            <td class=docs></td>
            <td class=code></td>
          </tr>
        </thead>
        <tbody>`, title)

    // Print HTML docs/code segments.
    for _, seg := range segs {
        fmt.Printf(
          `<tr>
             <td class=docs>%s</td>
             <td class=code>%s</td>
           </tr>`, seg.docsRendered, seg.codeRendered)
    }

    // Print HTML footer.
    fmt.Print(`</tbody>
           </table>
         </div>
       </body>
     </html>`)
}
