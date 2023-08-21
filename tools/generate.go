package main

import (
	"bytes"
	"crypto/sha1"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"

	"github.com/alecthomas/chroma/v2"
	"github.com/alecthomas/chroma/v2/formatters/html"
	"github.com/alecthomas/chroma/v2/lexers"
	"github.com/alecthomas/chroma/v2/styles"

	"github.com/russross/blackfriday/v2"
)

// siteDir is the target directory into which the HTML gets generated. Its
// default is set here but can be changed by an argument passed into the
// program.
var siteDir = "./public"

func verbose() bool {
	return len(os.Getenv("VERBOSE")) > 0
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func isDir(path string) bool {
	fileStat, _ := os.Stat(path)
	return fileStat.IsDir()
}

func ensureDir(dir string) {
	err := os.MkdirAll(dir, 0755)
	check(err)
}

func copyFile(src, dst string) {
	dat, err := os.ReadFile(src)
	check(err)
	err = os.WriteFile(dst, dat, 0644)
	check(err)
}

func sha1Sum(s string) string {
	h := sha1.New()
	h.Write([]byte(s))
	b := h.Sum(nil)
	return fmt.Sprintf("%x", b)
}

func mustReadFile(path string) string {
	bytes, err := os.ReadFile(path)
	check(err)
	return string(bytes)
}

func markdown(src string) string {
	return string(blackfriday.Run([]byte(src)))
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

var docsPat = regexp.MustCompile(`^(\s*(\/\/|#)\s|\s*\/\/$)`)
var dashPat = regexp.MustCompile(`\-+`)

// Seg is a segment of an example
type Seg struct {
	Docs, DocsRendered              string
	Code, CodeRendered, CodeForJs   string
	CodeEmpty, CodeLeading, CodeRun bool
}

// Example is info extracted from an example file
type Example struct {
	ID, Name                    string
	GoCode, GoCodeHash, URLHash string
	Segs                        [][]*Seg
	PrevExample                 *Example
	NextExample                 *Example
}

func parseHashFile(sourcePath string) (string, string) {
	lines := readLines(sourcePath)
	return lines[0], lines[1]
}

func resetURLHashFile(codehash, code, sourcePath string) string {
	if verbose() {
		fmt.Println("  Sending request to play.golang.org")
	}
	payload := strings.NewReader(code)
	resp, err := http.Post("https://play.golang.org/share", "text/plain", payload)
	check(err)
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	check(err)
	urlkey := string(body)
	data := fmt.Sprintf("%s\n%s\n", codehash, urlkey)
	os.WriteFile(sourcePath, []byte(data), 0644)
	return urlkey
}

func parseSegs(sourcePath string) ([]*Seg, string) {
	var (
		lines  []string
		source []string
	)
	// Convert tabs to spaces for uniform rendering.
	for _, line := range readLines(sourcePath) {
		lines = append(lines, strings.Replace(line, "\t", "    ", -1))
		source = append(source, line)
	}
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
				lastSeg := segs[len(segs)-1]
				if len(lastSeg.Code) == 0 {
					lastSeg.Code = line
				} else {
					lastSeg.Code = lastSeg.Code + "\n" + line
				}
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
	return segs, strings.Join(source, "\n")
}

func chromaFormat(code, filePath string) string {
	lexer := lexers.Get(filePath)
	if lexer == nil {
		lexer = lexers.Fallback
	}

	if strings.HasSuffix(filePath, ".sh") {
		lexer = SimpleShellOutputLexer
	}

	lexer = chroma.Coalesce(lexer)

	style := styles.Get("swapoff")
	if style == nil {
		style = styles.Fallback
	}
	formatter := html.New(html.WithClasses(true))
	iterator, err := lexer.Tokenise(nil, string(code))
	check(err)
	buf := new(bytes.Buffer)
	err = formatter.Format(buf, style, iterator)
	check(err)
	return buf.String()
}

func parseAndRenderSegs(sourcePath string) ([]*Seg, string) {
	segs, filecontent := parseSegs(sourcePath)
	lexer := whichLexer(sourcePath)
	for _, seg := range segs {
		if seg.Docs != "" {
			seg.DocsRendered = markdown(seg.Docs)
		}
		if seg.Code != "" {
			seg.CodeRendered = chromaFormat(seg.Code, sourcePath)

			// adding the content to the js code for copying to the clipboard
			if strings.HasSuffix(sourcePath, ".go") {
				seg.CodeForJs = strings.Trim(seg.Code, "\n") + "\n"
			}
		}
	}
	// we are only interested in the 'go' code to pass to play.golang.org
	if lexer != "go" {
		filecontent = ""
	}
	return segs, filecontent
}

func parseExamples() []*Example {
	var exampleNames []string
	for _, line := range readLines("examples.txt") {
		if line != "" && !strings.HasPrefix(line, "#") {
			exampleNames = append(exampleNames, line)
		}
	}
	examples := make([]*Example, 0)
	for i, exampleName := range exampleNames {
		if verbose() {
			fmt.Printf("Processing %s [%d/%d]\n", exampleName, i+1, len(exampleNames))
		}
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
			if !isDir(sourcePath) {
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
		}
		newCodeHash := sha1Sum(example.GoCode)
		if example.GoCodeHash != newCodeHash {
			example.URLHash = resetURLHashFile(newCodeHash, example.GoCode, "examples/"+example.ID+"/"+example.ID+".hash")
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

func renderIndex(examples []*Example) {
	if verbose() {
		fmt.Println("Rendering index")
	}
	indexTmpl := template.New("index")
	template.Must(indexTmpl.Parse(mustReadFile("templates/footer.tmpl")))
	template.Must(indexTmpl.Parse(mustReadFile("templates/index.tmpl")))
	indexF, err := os.Create(siteDir + "/index.html")
	check(err)
	defer indexF.Close()
	check(indexTmpl.Execute(indexF, examples))
}

func renderExamples(examples []*Example) {
	if verbose() {
		fmt.Println("Rendering examples")
	}
	exampleTmpl := template.New("example")
	template.Must(exampleTmpl.Parse(mustReadFile("templates/footer.tmpl")))
	template.Must(exampleTmpl.Parse(mustReadFile("templates/example.tmpl")))
	for _, example := range examples {
		exampleF, err := os.Create(siteDir + "/" + example.ID)
		check(err)
		defer exampleF.Close()
		check(exampleTmpl.Execute(exampleF, example))
	}
}

func render404() {
	if verbose() {
		fmt.Println("Rendering 404")
	}
	tmpl := template.New("404")
	template.Must(tmpl.Parse(mustReadFile("templates/footer.tmpl")))
	template.Must(tmpl.Parse(mustReadFile("templates/404.tmpl")))
	file, err := os.Create(siteDir + "/404.html")
	check(err)
	defer file.Close()
	check(tmpl.Execute(file, ""))
}

func main() {
	if len(os.Args) > 1 {
		siteDir = os.Args[1]
	}
	ensureDir(siteDir)

	copyFile("templates/site.css", siteDir+"/site.css")
	copyFile("templates/site.js", siteDir+"/site.js")
	copyFile("templates/favicon.ico", siteDir+"/favicon.ico")
	copyFile("templates/play.png", siteDir+"/play.png")
	copyFile("templates/clipboard.png", siteDir+"/clipboard.png")
	examples := parseExamples()
	renderIndex(examples)
	renderExamples(examples)
	render404()
}

var SimpleShellOutputLexer = chroma.MustNewLexer(
	&chroma.Config{
		Name:      "Shell Output",
		Aliases:   []string{"console"},
		Filenames: []string{"*.sh"},
		MimeTypes: []string{},
	},
	func() chroma.Rules {
		return chroma.Rules{
			"root": {
				// $ or > triggers the start of prompt formatting
				{`^\$`, chroma.GenericPrompt, chroma.Push("prompt")},
				{`^>`, chroma.GenericPrompt, chroma.Push("prompt")},

				// empty lines are just text
				{`^$\n`, chroma.Text, nil},

				// otherwise its all output
				{`[^\n]+$\n?`, chroma.GenericOutput, nil},
			},
			"prompt": {
				// when we find newline, do output formatting rules
				{`\n`, chroma.Text, chroma.Push("output")},
				// otherwise its all text
				{`[^\n]+$`, chroma.Text, nil},
			},
			"output": {
				// sometimes there isn't output so we go right back to prompt
				{`^\$`, chroma.GenericPrompt, chroma.Pop(1)},
				{`^>`, chroma.GenericPrompt, chroma.Pop(1)},
				// otherwise its all output
				{`[^\n]+$\n?`, chroma.GenericOutput, nil},
			},
		}
	},
)
