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

var cacheDir = "/tmp/gobyexample-cache"
var siteDir = "site"

func check(err error) {
    if err != nil {
        panic(err)
    }
}

func ensureDir(dir string) {
    err := os.MkdirAll(dir, 0700)
    check(err)
}

func pipe(bin string, arg []string, src string) []byte {
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

func cachedPygmentize(lex string, src string) string {
    ensureDir(cacheDir)
    arg := []string{"-l", lex, "-f", "html"}
    bin := "/usr/local/bin/pygmentize"
    cachePath := cacheDir + "/pygmentize-" + strings.Join(arg, "-") + "-" + sha1Sum(src)
    cacheBytes, cacheErr := ioutil.ReadFile(cachePath)
    if cacheErr == nil {
        return string(cacheBytes)
    }
    renderBytes := pipe(bin, arg, src)
    writeErr := ioutil.WriteFile(cachePath, renderBytes, 0600)
    check(writeErr)
    return string(renderBytes)
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

type Seg struct {
    docs, code, docsRendered, codeRendered string
}

type Chapter struct {
    id, name string
    segs     []*Seg
}

func parseSegs(sourcePath string) []*Seg {
    lines := readLines(sourcePath)
    segs := []*Seg{}
    segs = append(segs, &Seg{code: "", docs: ""})
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
                newSeg := Seg{docs: trimmed, code: ""}
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
                newSeg := Seg{docs: trimmed, code: ""}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.docs = lastSeg.docs + "\n" + trimmed
            }
            debug("DOCS")
            lastSeen = "docs"
        } else {
            if newCode {
                newSeg := Seg{docs: "", code: line}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.code = lastSeg.code + "\n" + line
            }
            debug("CODE")
            lastSeen = "code"
        }
    }
    return append(segs, &Seg{code: "", docs: ""})
}

func parseAndRenderSegs(sourcePath string) []*Seg {
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

func parseChapters() []*Chapter {
    chapterLines := readLines("meta/chapters.txt")
    chapters := make([]*Chapter, 0)
    for _, chapterId := range chapterLines {
        if (chapterId != "") && !strings.HasPrefix(chapterId, "#") {
            chapter := Chapter{id: chapterId}
            chapterLines := readLines("src/" + chapterId + "/" + chapterId + ".go")
            chapter.name = chapterLines[0][6:]
            chapterPath := "src/" + chapterId
            sourcePaths := mustGlob(chapterPath + "/*")
            segs := []*Seg{}
            for _, sourcePath := range sourcePaths {
                sourceSegs := parseAndRenderSegs(sourcePath)
                segs = append(segs, sourceSegs...)
            }
            chapter.segs = segs
            chapters = append(chapters, &chapter)
        }
    }
    return chapters
}

func renderIndex(chapters []*Chapter) {
    indexF, err := os.Create(siteDir + "/index.html")
    check(err)
    fmt.Fprint(indexF,
        `<!DOCTYPE html>
         <html>
           <head>
             <meta http-eqiv="content-type" content="text/html;charset=utf-8">
             <title>Go by Example</title>
             <link rel=stylesheet href="../style/site.css">
           </head>
           <body>
           <div id="intro">
             <h2>Go by Example</h2>
             <ul>`)

    for _, chapter := range chapters {
        fmt.Fprintf(indexF, `<li><a href="%s.html">%s</a></li>`, chapter.id, chapter.name)
    }
    fmt.Fprint(indexF, `</ul></div></body></html>`)
}

func renderChapters(chapters []*Chapter) {
    for _, chapter := range chapters {
        chapterF, err := os.Create(siteDir + "/" + chapter.id + ".html")
        check(err)
        fmt.Fprintf(chapterF,
            `<!DOCTYPE html>
             <html>
               <head>
                 <meta http-eqiv="content-type" content="text/html;charset=utf-8">
                 <title>Go by Example: %s</title>
                 <link rel=stylesheet href="../style/site.css">
               </head>
               <body>
                 <div class="chapter" id="%s">
                   <table cellspacing="0" cellpadding="0"><tbody>`,
            chapter.name, chapter.id)
        for _, seg := range chapter.segs {
            codeClasses := "code"
            if seg.code == "" {
                codeClasses = codeClasses + " empty"
            }
            fmt.Fprintf(chapterF,
                `<tr>
                 <td class=docs>%s</td>
                 <td class="%s">%s</td>
                 </tr>`,
                seg.docsRendered, codeClasses, seg.codeRendered)
        }
        fmt.Fprint(chapterF, `</tbody></table></div></body></html>`)
    }
}

func main() {
    ensureDir(siteDir)
    chapters := parseChapters()
    renderIndex(chapters)
    renderChapters(chapters)
}
