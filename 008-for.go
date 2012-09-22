package main                   // `for` is Go's only looping construct. Below are
                               // two common forms.
import "fmt"                    
                                
func main() {                   
    i := 1                     // Initialize `i` with `1` and loop until it's 10.
    for i <= 3 {               
        fmt.Print(i)          
        i = i + 1               
    }                           
                                
    for j := 1; j <= 3; j++ {  // That type of loop is common. We can do it on one
        fmt.Print(j)           // line.
    }

    for {                      // `for` without a condition will loop until you
        fmt.Println()          // `return`.
        return                 
    }                          
}                              // We'll see other `for` forms latter.

/*
$ go run for.go
123123
*/

// == todo
// break out of for loop?
