package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "os/exec"
    "regexp"
    "strings"
    "github.com/russross/blackfriday"
)

func check(err error) {
    if err != nil {
        panic(err)
    }
}

func render(bin string, arg []string, src string) string {
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

func readLines(path string) []string {
    srcBytes, err := ioutil.ReadFile(path)
    check(err)
    return strings.Split(string(srcBytes), "\n")
}

func whichLexer(path string) string {
    if strings.HasSuffix(path, ".go") {
        return "go"
    } else if strings.HasSuffix(path, ".sh") {
        return "console"
    }
    panic("No lexer for " + path)
    return ""
}

func progress(msg string) {
    if os.Getenv("DEBUG") != "1" {
        fmt.Fprint(os.Stderr, msg)
    }
}

func debug(msg string) {
    if os.Getenv("DEBUG") == "1" {
        fmt.Fprintln(os.Stderr, msg)
    }
}

var docsPat = regexp.MustCompile("^\\s*(\\/\\/|#)\\s")
var headerPat = regexp.MustCompile("^\\s*(\\/\\/|#)\\s#+\\s")
var todoPat = regexp.MustCompile("\\/\\/ todo: ")

type seg struct {
    docs, code, docsRendered, codeRendered string
}

func main() {
    if len(os.Args) <= 1 {
        fmt.Fprintln(os.Stderr, "usage: tool/build-html-inner *.{go,sh} > output.html")
        os.Exit(1)
    }

    fmt.Print(`<!DOCTYPE html>
                <html>
                  <head>
                    <meta http-eqiv="content-type" content="text/html;charset=utf-8">
                    <title>Go by Example</title>
                    <link rel=stylesheet href="../style/book.css">
                  </head>
                  <body>
                    <div id="container">
                      <div id="background"></div>
                      <table cellspacing="0" cellpadding="0">
                        <tbody>`)

    for _, sourcePath := range os.Args[1:] {
        progress(".")

        lexer := whichLexer(sourcePath)
        lines := readLines(sourcePath)

        segs := []*seg{}
        segs = append(segs, &seg{code: "", docs: ""})
        lastSeen := ""
        for _, line := range lines {
            if todoPat.MatchString(line) {
                continue
            }
            headerMatch := headerPat.MatchString(line)
            docsMatch := docsPat.MatchString(line)
            emptyMatch := line == ""
            lastSeg := segs[len(segs)-1]
            lastHeader := lastSeen == "header"
            lastDocs := lastSeen == "docs"
            newHeader := lastSeen != "header" && lastSeg.docs != ""
            newDocs := lastSeen == "code" || lastSeen == "header"
            newCode := (lastSeen != "code" && lastSeg.code != "") || lastSeen == "header"
            if newHeader || newDocs || newCode {
                debug("NEWSEG")
            }
            if headerMatch || (emptyMatch && lastHeader) {
                trimmed := docsPat.ReplaceAllString(line, "")
                if newHeader {
                    newSeg := seg{docs: trimmed, code: ""}
                    segs = append(segs, &newSeg)
                } else {
                    lastSeg.docs = lastSeg.docs + "\n" + trimmed
                }
                debug("HEAD")
                lastSeen = "header"
            } else if docsMatch || (emptyMatch && lastDocs) {
                trimmed := docsPat.ReplaceAllString(line, "")
                if newDocs {
                    debug("NEWSEG")
                    newSeg := seg{docs: trimmed, code: ""}
                    segs = append(segs, &newSeg)
                } else {
                    lastSeg.docs = lastSeg.docs + "\n" + trimmed
                }
                debug("DOCS")
                lastSeen = "docs"
            } else {
                if newCode {
                    newSeg := seg{docs: "", code: line}
                    segs = append(segs, &newSeg)
                } else {
                    lastSeg.code = lastSeg.code + "\n" + line
                }
                debug("CODE")
                lastSeen = "code"
            }
        }
        segs = append(segs, &seg{code: "", docs: ""})

        for _, seg := range segs {
            if seg.docs != "" {
                seg.docsRendered = string(blackfriday.MarkdownCommon([]byte(seg.docs)))
            }
            if seg.code != "" {
                seg.codeRendered = render("/usr/local/bin/pygmentize", []string{"-l", lexer, "-f", "html"}, seg.code)
            }
        }

        for _, seg := range segs {
            codeClasses := "code"
            if seg.code == "" {
                codeClasses = codeClasses + " empty"
            }
            fmt.Printf(
                `<tr>
				 <td class=docs>%s</td>
				 <td class="%s">%s</td>
				 </tr>`, seg.docsRendered, codeClasses, seg.codeRendered)
        }
    }

    fmt.Print(`</tbody></table></div></body></html>`)
}
