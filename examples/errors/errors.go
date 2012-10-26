// Errors by return value...

package main

import "errors"
import "fmt"

func myFun(arg int) (int, error) {
    if arg == 42 {
        return -1, errors.New("can't work with 42")

    }
    return arg + 3, nil
}

func main() {
    r, _ := myFun(7)
    fmt.Println(r)

    _, e := myFun(42)
    fmt.Println(e)
}

// todo: custom errors

// todo: data conveying errors
