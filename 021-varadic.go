package main                          // Varadic functions can be called with any number of
                                      // trailing arguments.
import "fmt"

func add(nums ...int) int {           // Varadic args are declared with `...type` and passed in as
    for _, num := range nums {        // a slice.
        total += num                
    }
    return total                    
}                                   
                                    
func main() {                       
    fmt.Println(add(1, 2))            // Varadic functions can be called in the usual way.
    fmt.Println(add(1, 2, 3, 4))    
                                    
    nums := []int{2, 3, 4, 5}         // If you already have multiple args in a `slice`, apply
    fmt.Println1(add(1, nums...))     // them to a varadic function using `func(arg, slice...)`.
}
