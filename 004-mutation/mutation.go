// ## Mutation

package main

import "fmt"

func main() {
    var x string
    x = "old"
    fmt.Println(x)

    // You can mutate variables in Go by re-assigning the
    // variable to a new value.
    x = "new"
    fmt.Println(x)
}
