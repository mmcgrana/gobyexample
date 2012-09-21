package main                        // The `math/rand` package provides psuedo-random
                                    // numbers.
import (
	"math/rand"
	"fmt"
)

func main() {
	fmt.Print(rand.Intn(100), ",")  // For example, `rand.Intn` returns a random `int` n,
	fmt.Print(rand.Intn(100))       // `0 <= n < 100`.
    fmt.Println()

	fmt.Println(rand.Float64())     // `rand.Float64` returns a `float64` `f`,
	                                // `0.0 <= f < 1.0`.
                                    
    s1 := rand.NewSource(42)        // To make the psuedo-random generator deterministic,
	r1 := rand.New(s1)              // give it a well-known seed.
											   
	
	fmt.Print(r1.Intn(100), ",")    // Call the resulting `rand.Source` just like the
	fmt.Print(r1.Intn(100))         // functions on the `rand` package.
    fmt.Println()

    s2 := rand.NewSource(42)        // If you seed a source with the same number, it
	r2 := rand.New(s2)              // produces the same sequence of random numbers.
	fmt.Print(r2.Intn(100), ",")                  
	fmt.Print(r2.Intn(100))
	fmt.Println()
}

/*
$ go run rand.go
81,87
0.6645600532184904
5,87
5,87
*/
