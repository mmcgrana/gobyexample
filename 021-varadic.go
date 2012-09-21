package main                   // Varadic functions can be called with any number of
                               // trailing arguments.
import "fmt"

func add(nums ...int) int {    // Varadic args are declared with `...type` and
    fmt.Print(nums, " ")       // passed in as a slice.
    total := 0
    for _, num := range nums {        
        total += num                
    }
    return total                    
}                                   
                                    
func main() {                       
    fmt.Println(add(1, 2))     // Varadic functions can be called in the usual way.
    fmt.Println(add(1, 2, 3))    
                                    
    nums := []int{1, 2, 3, 4}  // If you already have multiple args in a slice,
    fmt.Println(add(nums...))  // apply them to a varadic function using `
}                              // func(slice...)`.

/*
$ go run varadic.go
[1 2] 3
[1 2 3] 6
[1 2 3 4] 10
*/
