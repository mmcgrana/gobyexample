// Generate an HTML file from the book source. Derived from golit.

package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "os/exec"
    "regexp"
    "strings"
)

func check(err error) {
    if err != nil {
        panic(err)
    }
}

func pipe(bin string, arg []string, src string) string {
    cmd := exec.Command(bin, arg...)
    in, _ := cmd.StdinPipe()
    out, _ := cmd.StdoutPipe()
    cmd.Start()
    in.Write([]byte(src))
    in.Close()
    bytes, _ := ioutil.ReadAll(out)
    err := cmd.Wait()
    check(err)
    return string(bytes)
}

var docsPat = regexp.MustCompile("^\\s*\\/\\/\\s")

var headerPat = regexp.MustCompile("^\\s*\\/\\/\\s#+\\s")

type seg struct {
    docs, code, docsRendered, codeRendered string
}

func main() {
    if len(os.Args) != 3 {
        fmt.Fprintln(os.Stderr, "usage: tool/generate input.go title > output.html")
        os.Exit(1)
    }
    sourcePath := os.Args[1]
    title := os.Args[2]

    markdownPath, err := exec.LookPath("markdown")
    check(err)
    pygmentizePath, err := exec.LookPath("pygmentize")
    check(err)

    srcBytes, err := ioutil.ReadFile(sourcePath)
    check(err)
    lines := strings.Split(string(srcBytes), "\n")

    segs := []*seg{}
    segs = append(segs, &seg{code: "", docs: ""})
    lastSeen := ""
    for _, line := range lines {
        headerMatch := headerPat.MatchString(line)
        docsMatch := docsPat.MatchString(line)
        emptyMatch := line == ""
        lastSeg := segs[len(segs)-1]
        lastHeader := lastSeen == "header"
        lastDocs := lastSeen == "docs"
        newHeader := (lastSeen != "header") && lastSeg.docs != ""
        newDocs := (lastSeen != "docs") && lastSeg.docs != ""
        newCode := (lastSeen != "code") && lastSeg.code != ""
        if headerMatch || (emptyMatch && lastHeader) {
            trimmed := docsPat.ReplaceAllString(line, "")
            if newHeader {
                newSeg := seg{docs: trimmed, code: ""}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.docs = lastSeg.docs + "\n" + trimmed
            }
			lastSeen = "header"
        } else if docsMatch || (emptyMatch && lastDocs) {
            trimmed := docsPat.ReplaceAllString(line, "")
            if newDocs {
                newSeg := seg{docs: trimmed, code: ""}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.docs = lastSeg.docs + "\n" + trimmed
            }
            lastSeen = "docs"
        } else {
            if newCode {
                newSeg := seg{docs: "", code: line}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.code = lastSeg.code + "\n" + line
            }
            lastSeen = "code"
        }
    }

    for _, seg := range segs {
        seg.docsRendered = pipe(markdownPath, []string{}, seg.docs)
        seg.codeRendered = pipe(pygmentizePath, []string{"-l", "go", "-f", "html"}, seg.code+"  ")
    }

    fmt.Printf(`<!DOCTYPE html>
                <html>
                  <head>
                    <meta http-eqiv="content-type" content="text/html;charset=utf-8">
                    <title>%s</title>
                    <link rel=stylesheet href="../style/book.css">
                  </head>
                  <body>
                    <div id="container">
                      <div id="background"></div>
                      <table cellspacing="0" cellpadding="0">
                        <tbody>`, title)

    for _, seg := range segs {
        fmt.Printf(
            `<tr>
             <td class=docs>%s</td>
             <td class=code>%s</td>
             </tr>`, seg.docsRendered, seg.codeRendered)
    }

    fmt.Print(`</tbody></table></div></body></html>`)
}
