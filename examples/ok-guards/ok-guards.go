package main

import "fmt"

func main() {
    x := make(map[string]string)
    x["here"] = "yes"

    if name, ok := x["?"]; ok {
        fmt.Println(name)
    } else {
        fmt.Println("miss")
    }

    if name, ok := x["here"]; ok {
        fmt.Println(name)
    }
}

// todo: note about use with errors
