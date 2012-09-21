                                                    // The `math/rand` package provides psuedo-random numbers.

package main

import (
	"math/rand"
	"fmt"
)

func main() {
	fmt.Println(rand.Intn(100))                     // For example, `rand.Intn` returns a random `int` n,
	fmt.Println(rand.Intn(100))                     // `0 <= n < 100`.


	fmt.Println(rand.Float64())                     // `rand.Float64` returns a `float64` `f`, `0.0 <= f < 1.0`.

	r1 := rand.New(rand.NewSource(int64(1337)))     // To make the psuedo-random generator deterministic, give it a
												    // well-known seed.
	
	fmt.Println(r1.Intn(100))                       // Call the resulting `rand.Source` just like the functions on
	fmt.Println(r1.Intn(100))                       // the `rand` package.

	r2 := rand.New(rand.NewSource(int64(1337)))     // If you seed a source with the same number, it produces the
	fmt.Println(r2.Intn(100))                       // same sequence of random numbers.
	fmt.Println(r2.Intn(100))
}
