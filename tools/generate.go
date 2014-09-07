package main

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/csv"
	"encoding/gob"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"

	"github.com/russross/blackfriday"
)

var cacheDir = "."
var siteDir = "./public"

var pygmentizeBin = "./tools/pygmentize"

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
	debug(fmt.Sprintln("**", bin, arg, "\n", src))
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

var cacheData map[string]string

func loadCache(cacheFileName string) {
	cacheData = make(map[string]string)
	var b bytes.Buffer
	f, err := os.Open(cacheFileName)
	if err != nil { // skip error
		fmt.Println("Ignore cache:", err)
		return
	}
	_, err = io.Copy(&b, f)
	check(err)
	dec := gob.NewDecoder(&b)
	err = dec.Decode(&cacheData)
	check(err)
}
func saveCache(cacheFileName string) {
	var b bytes.Buffer
	enc := gob.NewEncoder(&b)
	err := enc.Encode(cacheData)
	check(err)
	f, err := os.Create(cacheFileName)
	check(err)
	_, err = io.Copy(f, &b)
	check(err)
}

// rm -rf /tmp/gobyexample-cache, use disk to cache: 235.83
// rm -rf /tmp/gobyexample-cache/gob, use memory to cache: 231.59
func cachedPygmentize(lex string, src string) string {
	ensureDir(cacheDir)
	arg := []string{"-l", lex, "-f", "html"}

	cachePath := cacheDir + "/pygmentize-" + strings.Join(arg, "-") + "-" + sha1Sum(src)
	/*
		cacheBytes, cacheErr := ioutil.ReadFile(cachePath)
		if cacheErr == nil {
			return string(cacheBytes)
		}
	*/
	if cache, ok := cacheData[cachePath]; ok {
		return cache
	}
	renderBytes := pipe(pygmentizeBin, arg, src)
	/*
		writeErr := ioutil.WriteFile(cachePath, renderBytes, 0600)
		check(writeErr)
	*/
	cacheData[cachePath] = string(renderBytes)
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
var dashPat = regexp.MustCompile("\\-+")

type Seg struct {
	Docs, DocsRendered              string
	Code, CodeRendered              string
	CodeEmpty, CodeLeading, CodeRun bool
}

type Example struct {
	Id, Name, Category, Author  string
	GoCode, GoCodeHash, UrlHash string
	Segs                        [][]*Seg
	PrevExample                 *Example
	NextExample                 *Example
}

type IndexData struct {
	Seqs       []*Example
	Categories map[string][]*Example
	Authors    map[string][]*Example
}

func parseHashFile(sourcePath string) (string, string) {
	lines := readLines(sourcePath)
	return lines[0], lines[1]
}

func resetUrlHashFile(codehash, code, sourcePath string) string {
	payload := strings.NewReader(code)
	resp, err := http.Post("http://play.golang.org/share", "text/plain", payload)
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
		if todoPat.MatchString(line) {
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

func loadCSV(csvFileName string) [][]string {
	fmt.Println("loading ...", csvFileName)
	f, err := os.Open(csvFileName)
	check(err)
	defer f.Close()
	r := bufio.NewReader(f)
	// skip BOM
	bom, err := r.Peek(3)
	check(err)
	if bom[0] == 0xEF && bom[1] == 0xBB && bom[2] == 0xBF {
		r.ReadByte()
		r.ReadByte()
		r.ReadByte()
	}
	csvR := csv.NewReader(r)
	csvR.FieldsPerRecord = -1 // skip checking fields count
	csvR.TrimLeadingSpace = true
	out, err := csvR.ReadAll()
	check(err)
	var result [][]string
	for _, line := range out {
		if line[0][0] == '#' { // remove # comment lines
			continue
		}
		if len(line) != 3 {
			fmt.Println("Err: invalid csv format on line", line)
			os.Exit(1)
		}
		result = append(result, line)
	}
	return result
}
func parseExamples() []*Example {
	lines := loadCSV("examples.csv")
	examples := make([]*Example, 0)
	for _, line := range lines {
		cat, author, exampleName := line[0], line[1], line[2]
		example := Example{Name: exampleName, Category: cat, Author: author}
		exampleId := strings.ToLower(exampleName)
		exampleId = strings.Replace(exampleId, " ", "-", -1)
		exampleId = strings.Replace(exampleId, "/", "-", -1)
		exampleId = strings.Replace(exampleId, "'", "", -1)
		exampleId = dashPat.ReplaceAllString(exampleId, "-")
		example.Id = exampleId
		example.Segs = make([][]*Seg, 0)
		fmt.Println("Generating:", exampleId)
		sourcePaths := mustGlob("examples/" + exampleId + "/*")
		if len(sourcePaths) < 2 {
			fmt.Println("Err: missing .go or .sh in", exampleId,
				":", sourcePaths)
			os.Exit(1)
		}
		for _, sourcePath := range sourcePaths {
			if strings.HasSuffix(sourcePath, ".hash") {
				example.GoCodeHash, example.UrlHash = parseHashFile(sourcePath)
			} else if strings.HasPrefix(sourcePath, ".") {
				continue // skip .*.swp files
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
			example.UrlHash = resetUrlHashFile(newCodeHash, example.GoCode, "examples/"+example.Id+"/"+example.Id+".hash")
		}
		examples = append(examples, &example)
	}
	for i, example := range examples {
		if i > 0 {
			example.PrevExample = examples[i-1]
		}
		if i < (len(examples) - 1) {
			example.NextExample = examples[i+1]
		}
	}
	return examples
}

func buildIndexData(exs []*Example) IndexData {
	d := IndexData{Seqs: exs,
		Categories: make(map[string][]*Example),
		Authors:    make(map[string][]*Example)}
	for _, ex := range exs {
		if m, ok := d.Categories[ex.Category]; ok {
			d.Categories[ex.Category] = append(m, ex)
		} else {
			m := make([]*Example, 0)
			d.Categories[ex.Category] = append(m, ex)
		}
		if m, ok := d.Authors[ex.Author]; ok {
			d.Authors[ex.Author] = append(m, ex)
		} else {
			m := make([]*Example, 0)
			d.Authors[ex.Author] = append(m, ex)
		}
	}
	return d
}

func renderIndex(indexData IndexData) {
	tmpl := template.Must(template.ParseFiles("templates/index.tmpl"))
	indexF, err := os.Create(siteDir + "/index.html")
	check(err)
	err = tmpl.ExecuteTemplate(indexF, "index.tmpl", indexData)
	check(err)
}

func renderExamples(examples []*Example) {
	tmpl := template.Must(template.ParseFiles("templates/example.tmpl"))
	for _, example := range examples {
		exampleF, err := os.Create(siteDir + "/" + example.Id + ".html")
		check(err)
		err = tmpl.ExecuteTemplate(exampleF, "example.tmpl", example)
		check(err)
	}
}

func main() {
	loadCache(cacheDir + "/cache.gob")
	copyFile("templates/site.css", siteDir+"/site.css")
	copyFile("templates/favicon.ico", siteDir+"/favicon.ico")
	copyFile("templates/404.html", siteDir+"/404.html")
	copyFile("templates/play.png", siteDir+"/play.png")
	examples := parseExamples()
	indexData := buildIndexData(examples)
	renderIndex(indexData)
	renderExamples(examples)
	saveCache(cacheDir + "/cache.gob")
}
