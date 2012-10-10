package main

// Package `strconv` provides the number parsing.
import "strconv"
import "fmt"

func main() {
    // `64` tells how many bits of precision to parse.
    f, _ := strconv.ParseFloat("1.234", 64)
    fmt.Println(f)

    // `0` means infer the base from the string.
    // `64` requires that the result fit in 64 bits.
    i, _ := strconv.ParseInt("123", 0, 64)
    println(i)

    // `ParseInt` will recognize hex-formatted numbers.
    d, _ := strconv.ParseInt("0x1b3e", 0, 64)
    println(d)

    // `Atoi` is a convenienice function for `int`
    // parsing.
    k, _ := strconv.Atoi("456")
    println(k)

    // Parse functions return an error on bad input.
    _, e := strconv.Atoi("wat")
    fmt.Println(e)
}
