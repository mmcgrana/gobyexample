package main

import (
	"crypto/sha1"
	"fmt"
	"github.com/russross/blackfriday"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"
)

var cacheDir = "/tmp/gobyexample-cache"
var siteDir = "./public"
var pygmentizeBin = "./vendor/pygments/pygmentize"

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func ensureDir(dir string) {
	err := os.MkdirAll(dir, 0755)
	check(err)
}

func copyFile(src, dst string) {
	dat, err := ioutil.ReadFile(src)
	check(err)
	err = ioutil.WriteFile(dst, dat, 0644)
	check(err)
}

func pipe(bin string, arg []string, src string) []byte {
	cmd := exec.Command(bin, arg...)
	in, err := cmd.StdinPipe()
	check(err)
	out, err := cmd.StdoutPipe()
	check(err)
	err = cmd.Start()
	check(err)
	_, err = in.Write([]byte(src))
	check(err)
	err = in.Close()
	check(err)
	bytes, err := ioutil.ReadAll(out)
	check(err)
	err = cmd.Wait()
	check(err)
	return bytes
}

func sha1Sum(s string) string {
	h := sha1.New()
	h.Write([]byte(s))
	b := h.Sum(nil)
	return fmt.Sprintf("%x", b)
}

func mustReadFile(path string) string {
	bytes, err := ioutil.ReadFile(path)
	check(err)
	return string(bytes)
}

func cachedPygmentize(lex string, src string) string {
	ensureDir(cacheDir)
	arg := []string{"-l", lex, "-f", "html"}
	cachePath := cacheDir + "/pygmentize-" + strings.Join(arg, "-") + "-" + sha1Sum(src)
	cacheBytes, cacheErr := ioutil.ReadFile(cachePath)
	if cacheErr == nil {
		return string(cacheBytes)
	}
	renderBytes := pipe(pygmentizeBin, arg, src)
	// Newer versions of Pygments add silly empty spans.
	renderCleanString := strings.Replace(string(renderBytes), "<span></span>", "", -1)
	writeErr := ioutil.WriteFile(cachePath, []byte(renderCleanString), 0600)
	check(writeErr)
	return renderCleanString
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
}

func debug(msg string) {
	if os.Getenv("DEBUG") == "1" {
		fmt.Fprintln(os.Stderr, msg)
	}
}

var docsPat = regexp.MustCompile("^\\s*(\\/\\/|#)\\s")
var dashPat = regexp.MustCompile("\\-+")

// Seg is a segment of an example
type Seg struct {
	Docs, DocsRendered              string
	Code, CodeRendered              string
	CodeEmpty, CodeLeading, CodeRun bool
}

// Example is info extracted from an example file
type Example struct {
	ID, Name                    string
	GoCode, GoCodeHash, URLHash string
	Segs                        [][]*Seg
	NextExample                 *Example
}

func parseHashFile(sourcePath string) (string, string) {
	lines := readLines(sourcePath)
	return lines[0], lines[1]
}

func resetURLHashFile(codehash, code, sourcePath string) string {
	payload := strings.NewReader(code)
	resp, err := http.Post("https://play.golang.org/share", "text/plain", payload)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	urlkey := string(body)
	data := fmt.Sprintf("%s\n%s\n", codehash, urlkey)
	ioutil.WriteFile(sourcePath, []byte(data), 0644)
	return urlkey
}

func parseSegs(sourcePath string) ([]*Seg, string) {
	lines := readLines(sourcePath)
	filecontent := strings.Join(lines, "\n")
	segs := []*Seg{}
	lastSeen := ""
	for _, line := range lines {
		if line == "" {
			lastSeen = ""
			continue
		}
		matchDocs := docsPat.MatchString(line)
		matchCode := !matchDocs
		newDocs := (lastSeen == "") || ((lastSeen != "docs") && (segs[len(segs)-1].Docs != ""))
		newCode := (lastSeen == "") || ((lastSeen != "code") && (segs[len(segs)-1].Code != ""))
		if newDocs || newCode {
			debug("NEWSEG")
		}
		if matchDocs {
			trimmed := docsPat.ReplaceAllString(line, "")
			if newDocs {
				newSeg := Seg{Docs: trimmed, Code: ""}
				segs = append(segs, &newSeg)
			} else {
				segs[len(segs)-1].Docs = segs[len(segs)-1].Docs + "\n" + trimmed
			}
			debug("DOCS: " + line)
			lastSeen = "docs"
		} else if matchCode {
			if newCode {
				newSeg := Seg{Docs: "", Code: line}
				segs = append(segs, &newSeg)
			} else {
				segs[len(segs)-1].Code = segs[len(segs)-1].Code + "\n" + line
			}
			debug("CODE: " + line)
			lastSeen = "code"
		}
	}
	for i, seg := range segs {
		seg.CodeEmpty = (seg.Code == "")
		seg.CodeLeading = (i < (len(segs) - 1))
		seg.CodeRun = strings.Contains(seg.Code, "package main")
	}
	return segs, filecontent
}

func parseAndRenderSegs(sourcePath string) ([]*Seg, string) {
	segs, filecontent := parseSegs(sourcePath)
	lexer := whichLexer(sourcePath)
	for _, seg := range segs {
		if seg.Docs != "" {
			seg.DocsRendered = markdown(seg.Docs)
		}
		if seg.Code != "" {
			seg.CodeRendered = cachedPygmentize(lexer, seg.Code)
		}
	}
	// we are only interested in the 'go' code to pass to play.golang.org
	if lexer != "go" {
		filecontent = ""
	}
	return segs, filecontent
}

func parseExamples() []*Example {
	exampleNames := readLines("examples.txt")
	examples := make([]*Example, 0)
	for _, exampleName := range exampleNames {
		if (exampleName != "") && !strings.HasPrefix(exampleName, "#") {
			example := Example{Name: exampleName}
			exampleID := strings.ToLower(exampleName)
			exampleID = strings.Replace(exampleID, " ", "-", -1)
			exampleID = strings.Replace(exampleID, "/", "-", -1)
			exampleID = strings.Replace(exampleID, "'", "", -1)
			exampleID = dashPat.ReplaceAllString(exampleID, "-")
			example.ID = exampleID
			example.Segs = make([][]*Seg, 0)
			sourcePaths := mustGlob("examples/" + exampleID + "/*")
			for _, sourcePath := range sourcePaths {
				if strings.HasSuffix(sourcePath, ".hash") {
					example.GoCodeHash, example.URLHash = parseHashFile(sourcePath)
				} else {
					sourceSegs, filecontents := parseAndRenderSegs(sourcePath)
					if filecontents != "" {
						example.GoCode = filecontents
					}
					example.Segs = append(example.Segs, sourceSegs)
				}
			}
			newCodeHash := sha1Sum(example.GoCode)
			if example.GoCodeHash != newCodeHash {
				example.URLHash = resetURLHashFile(newCodeHash, example.GoCode, "examples/"+example.ID+"/"+example.ID+".hash")
			}
			examples = append(examples, &example)
		}
	}
	for i, example := range examples {
		if i < (len(examples) - 1) {
			example.NextExample = examples[i+1]
		}
	}
	return examples
}

func renderIndex(examples []*Example) {
	indexTmpl := template.New("index")
	_, err := indexTmpl.Parse(mustReadFile("templates/index.tmpl"))
	check(err)
	indexF, err := os.Create(siteDir + "/index.html")
	check(err)
	err = indexTmpl.Execute(indexF, examples)
	check(err)
}

func renderExamples(examples []*Example) {
	exampleTmpl := template.New("example")
	_, err := exampleTmpl.Parse(mustReadFile("templates/example.tmpl"))
	check(err)
	for _, example := range examples {
		exampleF, err := os.Create(siteDir + "/" + example.ID)
		check(err)
		exampleTmpl.Execute(exampleF, example)
	}
}

func main() {
	copyFile("templates/site.css", siteDir+"/site.css")
	copyFile("templates/favicon.ico", siteDir+"/favicon.ico")
	copyFile("templates/404.html", siteDir+"/404.html")
	copyFile("templates/play.png", siteDir+"/play.png")
	examples := parseExamples()
	renderIndex(examples)
	renderExamples(examples)
}
