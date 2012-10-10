package main

import "flag"
import "fmt"

func main() {
    maxp := flag.Int("repeat", 3, "time to repeat args")
    flag.Parse()
    for i := 0; i < *maxp; i++ {
        for _, arg := range flag.Args() {
            fmt.Println(arg)
        }
    }
}

// todo: multiple flags
// todo: trailing args
// todo: arg escaping
// todo: help text and usage errors
