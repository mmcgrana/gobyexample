package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strings"
	"unicode/utf8"
)

const maxColumn = 58

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func readLines(path string) []string {
	srcBytes, err := ioutil.ReadFile(path)
	check(err)
	return strings.Split(string(srcBytes), "\n")
}

var commentPat = regexp.MustCompile("\\s*\\/\\/")

func main() {
	sourcePaths, err := filepath.Glob("./examples/*/*")
	check(err)
	foundLongFile := false
	for _, sourcePath := range sourcePaths {
		if strings.HasPrefix(path.Base(sourcePath), ".") {
			continue // skip .*.swp files
		}
		foundLongLine := false
		lines := readLines(sourcePath)
		for i, line := range lines {
			if !foundLongLine && !commentPat.MatchString(line) && (utf8.RuneCountInString(line) > maxColumn) {
				fmt.Printf("measure over %d column: %s:%d\n",
					maxColumn, sourcePath, i+1)
				foundLongLine = true
				foundLongFile = true
			}
		}
	}
	if foundLongFile {
		os.Exit(1)
	}
}
