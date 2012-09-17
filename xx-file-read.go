package main

import ("fmt"; "io/ioutil")

func main() {
    contents, err := ioutil.ReadFile("xx-file-read.go")
    if err != nil {
        return
    }
    fmt.Print(string(contents))
}
