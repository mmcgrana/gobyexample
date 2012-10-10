// Varadic functions can be called with any number of
// trailing arguments. This is useful if you don't know
// number of arguments that will be needed for a function
// ahead of time.

package main

import "fmt"

// Varadic args are declared with `...type` and
// passed in as a slice.
func add(nums ...int) int {
    fmt.Print(nums, " ")
    total := 0
    for _, num := range nums {
        total += num
    }
    return total
}

func main() {
    // Varadic functions can be called in the usual way.
    fmt.Println(add(1, 2))
    fmt.Println(add(1, 2, 3))

    // If you already have multiple args in a slice,
    // apply them to a varadic function using `
    // func(slice...)`.                              
    nums := []int{1, 2, 3, 4}
    fmt.Println(add(nums...))
}
