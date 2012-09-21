package main                    // `for` is Go's only looping construct. Below are
                                // two common forms.
import "fmt"                    
                                
func main() {                   
	i := 1                      // We initialize `i` with `1` and loop until it's 10.
	for i <= 10 {               
		fmt.Println(i)          
		i = i + 1               
	}                           
                                
	for j := 1; j <= 10; j++ {  // That type of loop is common. We can do it on one
		fmt.Println(j)          // line.
	}

    for {                       // `for` without a condition will loop until you`
        return                  // return`.
    }
}                               // We'll see other `for` forms latter.
