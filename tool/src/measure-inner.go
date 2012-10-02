package main

import (
    "fmt"
    "io/ioutil"
    "os"
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
    if len(os.Args) <= 1 {
        fmt.Fprintln(os.Stderr, "usage: tool/measure *.{go,sh}")
        os.Exit(1)
    }
    foundLongFile := false
    for _, sourcePath := range os.Args[1:] {
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
