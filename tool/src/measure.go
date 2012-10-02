package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "path/filepath"
    "strings"
)

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

func main() {
    sourcePaths, err := filepath.Glob("./src/0*/*")
    check(err)
    foundLongFile := false
    for _, sourcePath := range sourcePaths {
        foundLongLine := false
        lines := readLines(sourcePath)
        for _, line := range lines {
            if len(line) > 60 && !foundLongLine {
                fmt.Println(sourcePath)
                foundLongLine = true
                foundLongFile = true
            }
        }
    }
    if foundLongFile {
        os.Exit(1)
    }
}
