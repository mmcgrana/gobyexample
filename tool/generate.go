package main

import (
    "crypto/sha1"
    "encoding/hex"
    "fmt"
    "github.com/russross/blackfriday"
    "io/ioutil"
    "os"
    "os/exec"
    "path/filepath"
    "regexp"
    "strings"
)

var cacheDir = "/tmp/gbe-book-cache"

func check(err error) {
    if err != nil {
        panic(err)
    }
}

func filterStrings(vs []string, f func(string) bool) []string {
    vsf := make([]string, 0)
    for _, v := range vs {
        if f(v) {
            vsf = append(vsf, v)
        }
    }
    return vsf
}

func render(bin string, arg []string, src string) []byte {
    cmd := exec.Command(bin, arg...)
    in, _ := cmd.StdinPipe()
    out, _ := cmd.StdoutPipe()
    cmd.Start()
    in.Write([]byte(src))
    in.Close()
    bytes, _ := ioutil.ReadAll(out)
    err := cmd.Wait()
    check(err)
    return bytes
}

func sha1Sum(s string) string {
    h := sha1.New()
    h.Write([]byte(s))
    b := h.Sum(nil)
    return hex.EncodeToString(b)
}

func mustReadFile(path string) string {
    bytes, err := ioutil.ReadFile(path)
    check(err)
    return string(bytes)
}

func cachedRender(bin string, arg []string, src string) string {
    cachePath := cacheDir + "/pygmentize-" + strings.Join(arg, "-") + "-" + sha1Sum(src)
    cacheBytes, cacheErr := ioutil.ReadFile(cachePath)
    if cacheErr == nil {
        return string(cacheBytes)
    }
    renderBytes := render(bin, arg, src)
    writeErr := ioutil.WriteFile(cachePath, renderBytes, 0600)
    check(writeErr)
    return string(renderBytes)
}

func cachedPygmentize(lex string, src string) string {
    return cachedRender(
        "/usr/local/bin/pygmentize",
        []string{"-l", lex, "-f", "html"},
        src)
}

func ensureCache() {
    mkdirErr := os.MkdirAll(cacheDir, 0700)
    check(mkdirErr)
}

func markdown(src string) string {
    return string(blackfriday.MarkdownCommon([]byte(src)))
}

func readLines(path string) []string {
    src := mustReadFile(path)
    return strings.Split(src, "\n")
}

func mustGlob(glob string) []string {
    paths, err := filepath.Glob(glob)
    check(err)
    return paths
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

func parseSegs(sourcePath string) []*seg {
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
    return append(segs, &seg{code: "", docs: ""})
}

func parseAndRenderSegs(sourcePath string) []*seg {
    segs := parseSegs(sourcePath)
    lexer := whichLexer(sourcePath)
    for _, seg := range segs {
        if seg.docs != "" {
            seg.docsRendered = markdown(seg.docs)
        }
        if seg.code != "" {
            seg.codeRendered = cachedPygmentize(lexer, seg.code)
        }
    }
    return segs
}

func main() {
    if len(os.Args) != 2 {
        panic("Wrong number of args")
    }
    outF, err := os.Create(os.Args[1])
    check(err)
    ensureCache()

    // Header
    fmt.Fprint(outF,
        `<!DOCTYPE html>
        <html>
          <head>
            <meta http-eqiv="content-type" content="text/html;charset=utf-8">
            <title>Go by Example</title>
            <link rel=stylesheet href="../src/style.css">
          </head>
          <body>`)

    // Title page
    fmt.Fprintf(outF,
        `<div class="chapter" id="title">%s</div>`,
        markdown(mustReadFile("src/title.md")))

    // Contents page
    chapterIds := readLines("src/contents.txt")
    fmt.Fprint(outF, `<div class="chapter" id="contents"><h2>Contents</h2><ul>`)
    for _, chapterId := range chapterIds {
        var chapterName string
        if chapterId == "introduction" {
            chapterName = "Introduction"
        } else {
            chapterLines := readLines("src/" + chapterId + "/" + chapterId + ".go")
            chapterName = chapterLines[0][6:]
        }
        fmt.Fprintf(outF, `<li><a href="#%s">%s</a></li>`, chapterId, chapterName)
    }
    fmt.Fprint(outF, `</ul></div>`)

    // Content chapters
    for _, chapterId := range chapterIds {
        fmt.Fprintf(outF, `<div class="chapter" id="%s">`, chapterId)
        if chapterId == "introduction" {
            fmt.Fprint(outF, markdown(mustReadFile("src/introduction.md")))
        } else {
            chapterPath := "src/" + chapterId
            fmt.Fprintf(outF,
                `<table cellspacing="0" cellpadding="0" id="%s"><tbody>`,
                chapterPath)
            sourcePaths := mustGlob(chapterPath + "/*")
            for _, sourcePath := range sourcePaths {
                if strings.HasSuffix(sourcePath, ".go") || strings.HasSuffix(sourcePath, ".sh") {
                    segs := parseAndRenderSegs(sourcePath)
                    for _, seg := range segs {
                        codeClasses := "code"
                        if seg.code == "" {
                            codeClasses = codeClasses + " empty"
                        }
                        fmt.Fprintf(outF,
                            `<tr>
    		        		 <td class=docs>%s</td>
    		        		 <td class="%s">%s</td>
    		        		 </tr>`,
                            seg.docsRendered, codeClasses, seg.codeRendered)
                    }
                }
            }
            fmt.Fprint(outF, `</tbody></table>`)
        }
        fmt.Fprintf(outF, `</div>`)
    }

    // Footer
    fmt.Fprint(outF, `</body></html>`)
}
