package main

// The `math/rand` package provides psuedo-random
// numbers.
import "math/rand"
import "fmt"

func main() {
    // For example, `rand.Intn` returns a random `int` n,
    // `0 <= n < 100`.
    fmt.Print(rand.Intn(100), ",")
    fmt.Print(rand.Intn(100))
    fmt.Println()

    // `rand.Float64` returns a `float64` `f`,
    // `0.0 <= f < 1.0`.
    fmt.Println(rand.Float64())

    // To make the psuedo-random generator deterministic,
    // give it a well-known seed.
    s1 := rand.NewSource(42)
    r1 := rand.New(s1)

    // Call the resulting `rand.Source` just like the
    // functions on the `rand` package.
    fmt.Print(r1.Intn(100), ",")
    fmt.Print(r1.Intn(100))
    fmt.Println()

    // If you seed a source with the same number, it
    // produces the same sequence of random numbers.
    s2 := rand.NewSource(42)
    r2 := rand.New(s2)
    fmt.Print(r2.Intn(100), ",")
    fmt.Print(r2.Intn(100))
    fmt.Println()
}
