                          // A function can return multiple values.

package main

import "fmt"

func vals() (int, int) {  // The `(int, int)` in this signature shows that the
    return 3, 7           // function returns 2 ints.
    
}

func main() {
    x, y := vals()        // Use the 2 different return values from the call.
    fmt.Println(x)
    fmt.Println(y)
}

/*
$ go run 21-returns.go
3
7
*/
