// Renumber the .go content files according to the
// order in index.txt.

package main

import (
    "fmt"
    "io/ioutil"
    "strings"
    "regexp"
    "os"
    "sort"
)

func minInt(a, b int) int {
    if (a < b) {
        return a
    }
    return b
}

func main() {
    // read names of source files
    sourceNames := make([]string, 0)
    sourceMap := make(map[string]string)
    fileInfos, dirErr := ioutil.ReadDir("./")
    if dirErr != nil { panic(dirErr) }
    baseTrimmer, _ := regexp.Compile("[0-9x]+-")
    for _, fi := range fileInfos {
        baseName := baseTrimmer.ReplaceAllString(fi.Name(), "")
        if baseName != ".git" && baseName != "tool" && baseName != "README.md" {
            sourceNames = append(sourceNames, baseName)
            sourceMap[baseName] = fi.Name()
        }
    }

    // read names from index
    indexBytes, idxErr := ioutil.ReadFile("tool/index.txt")
    if idxErr != nil { panic (idxErr) }
    indexNamesAll := strings.Split(string(indexBytes), "\n")
    indexNames := make([]string, 0)
    for _, indexName := range indexNamesAll {
        if (indexName != "") && !strings.Contains(indexName, "#") {
            indexNames = append(indexNames, indexName)
        }
    }
    
    // sanity check two lists
    if len(sourceNames) != len(indexNames) {
        sort.Strings(sourceNames)
        sort.Strings(indexNames)
        for i := 0; i < minInt(len(sourceNames), len(indexNames)); i++ {
            fmt.Printf("%s %s\n", sourceNames[i], indexNames[i])
        }
        os.Exit(1)
    }
    
    // rename some stuff
    for index, indexName := range indexNames {
        oldName := sourceMap[indexName]
        newName := fmt.Sprintf("%03d-%s", index+1, indexName)
        if oldName != newName {
            os.Rename(oldName, newName)
        }
    }
}
