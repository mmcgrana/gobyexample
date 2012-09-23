package main                         // The `sort` package implements sorting for builtins
                                     // and user-defined types. We'll look at some of the
import "fmt"                         // sorts for builtins here.
import "sort"

func main() {
	strs := []string{"c", "a", "b"}
	sort.Strings(strs)               // Sort methods are specific to the builtin type.
	fmt.Println(strs)                // Sorting is in-place (doesn't return a new slice).

    ints := []int{7, 2, 4}
    sort.Ints(ints)                  // Sorting methods are named after the type.
    fmt.Println(ints)

    s := sort.IntsAreSorted(ints)    // Check if a slice is in sorted order.
    fmt.Println(s)
}

/*
$ go run sort.go
[a b c]
[2 4 7]
true
1
*/

// == todo
// general and convenience searching
