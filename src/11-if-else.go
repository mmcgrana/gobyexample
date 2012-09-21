                         // If/Else

package main

func main() {
    if 7 % 2 == 0 {      // If/else is straightford. Note that there are no
        println("even")  // enclosing parenthesis around the condition.
    } else {             // Also, there is no ternary operator (`?`) in Go.
        println("odd")
    }
}

/*
$ go run 11-if-else.go
odd
*/
