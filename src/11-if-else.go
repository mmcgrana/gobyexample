package main

import "fmt"

func main() {
    fmt.Print("7 is ")
    if 7 % 2 == 0 {          // If/else is straightford. Note that there are no
        fmt.Println("even")  // enclosing parenthesis around the condition.
    } else {                 // Also, there is no ternary operator (`?`) in Go.
        fmt.Println("odd")
    }
}

/*
$ go run 11-if-else.go
7 is odd
*/
