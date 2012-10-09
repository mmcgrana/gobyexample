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
    "text/template"
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
var todoPat = regexp.MustCompile("\\/\\/ todo: ")

type Seg struct {
    Docs, DocsRendered              string
    Code, CodeRendered, CodeClasses string
}

type Chapter struct {
    Id, Name string
    Segs     []*Seg
}

func parseSegs(sourcePath string) []*Seg {
    lines := readLines(sourcePath)
    segs := []*Seg{}
    segs = append(segs, &Seg{Code: "", Docs: ""})
    lastSeen := ""
    for _, line := range lines {
        if line == "" {
            lastSeen = ""
            continue
        }
        if todoPat.MatchString(line) {
            continue
        }
        matchDocs := docsPat.MatchString(line)
        matchCode := !matchDocs
        lastSeg := segs[len(segs)-1]
        newDocs := (lastSeen == "") || ((lastSeen != "docs") && (lastSeg.Docs != ""))
        newCode := (lastSeen == "") || ((lastSeen != "code") && (lastSeg.Code != ""))
        if newDocs || newCode {
            debug("NEWSEG")
        }
        if matchDocs {
            trimmed := docsPat.ReplaceAllString(line, "")
            if newDocs {
                newSeg := Seg{Docs: trimmed, Code: ""}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.Docs = lastSeg.Docs + "\n" + trimmed
            }
            debug("DOCS: " + line)
            lastSeen = "docs"
        } else if matchCode {
            if newCode {
                newSeg := Seg{Docs: "", Code: line}
                segs = append(segs, &newSeg)
            } else {
                lastSeg.Code = lastSeg.Code + "\n" + line
            }
            debug("CODE: " + line)
            lastSeen = "code"
        }
    }
    return append(segs, &Seg{Code: "", Docs: ""})
}

func parseAndRenderSegs(sourcePath string) []*Seg {
    segs := parseSegs(sourcePath)
    lexer := whichLexer(sourcePath)
    for _, seg := range segs {
        if seg.Docs != "" {
            seg.DocsRendered = markdown(seg.Docs)
        }
        if seg.Code != "" {
            seg.CodeRendered = cachedPygmentize(lexer, seg.Code)
        }
    }
    return segs
}

func parseChapters() []*Chapter {
    chapterLines := readLines("meta/chapters.txt")
    chapters := make([]*Chapter, 0)
    for _, chapterId := range chapterLines {
        if (chapterId != "") && !strings.HasPrefix(chapterId, "#") {
            chapter := Chapter{Id: chapterId}
            chapterLines := readLines("src/" + chapterId + "/" + chapterId + ".go")
            chapter.Name = chapterLines[0][6:]
            chapterPath := "src/" + chapterId
            sourcePaths := mustGlob(chapterPath + "/*")
            segs := []*Seg{}
            for _, sourcePath := range sourcePaths {
                sourceSegs := parseAndRenderSegs(sourcePath)
                segs = append(segs, sourceSegs...)
            }
            chapter.Segs = segs
            chapters = append(chapters, &chapter)
        }
    }
    return chapters
}

func renderIndex(chapters []*Chapter) {
    indexTmpl := template.New("index")
    _, err := indexTmpl.Parse(mustReadFile("template/index.tmpl"))
    check(err)
    indexF, err := os.Create(siteDir + "/index.html")
    check(err)
    indexTmpl.Execute(indexF, chapters)
}

func renderChapters(chapters []*Chapter) {
    chapterTmpl := template.New("chapter")
    _, err := chapterTmpl.Parse(mustReadFile("template/chapter.tmpl"))
    check(err)
    for _, chapter := range chapters {
        for _, seg := range chapter.Segs {
            seg.CodeClasses = "code"
            if seg.Code == "" {
                seg.CodeClasses = seg.CodeClasses + " empty"
            }
        }
        chapterF, err := os.Create(siteDir + "/" + chapter.Id + ".html")
        check(err)
        chapterTmpl.Execute(chapterF, chapter)
    }
}

func main() {
    ensureDir(siteDir)
    chapters := parseChapters()
    renderIndex(chapters)
    renderChapters(chapters)
}
