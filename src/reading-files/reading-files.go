package main

import "io/ioutil"
import "fmt"

func main() {
    contents, err := ioutil.ReadFile("xx-file-read.go")
    if err != nil {
        return
    }
    fmt.Print(string(contents))
}

// todo: streaming reads
