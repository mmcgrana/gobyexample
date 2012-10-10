package main

import "os"

func main() {
    file, err := os.Create("writing-files.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()
    file.WriteString("contents\n")
}

// todo: streaming writes
