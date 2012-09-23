// ## Sort

// The `sort` package implements sorting for builtins
// and user-defined types. We'll look at some of the
// sorts for builtins here.
package main                         
                                     
import "fmt"                         
import "sort"

func main() {
    // Sort methods are specific to the builtin type.
    // Sorting is in-place (doesn't return a new slice).
	strs := []string{"c", "a", "b"}
	sort.Strings(strs)               
	fmt.Println(strs)                

    // Sorting methods are named after the type.
    ints := []int{7, 2, 4}
    sort.Ints(ints)                  
    fmt.Println(ints)

    // Check if a slice is in sorted order.
    s := sort.IntsAreSorted(ints)    
    fmt.Println(s)
}

// todo: general and convenience searching
