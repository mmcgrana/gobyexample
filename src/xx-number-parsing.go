                                               // Number parsing

package main

import (
    "fmt"
    "strconv"                                  // Package `strconv` provides number parsing.
)

func main() {
	f, _ := strconv.ParseFloat("1.234", 64)    // `64` tells how many bits of precision to parse.
	fmt.Println(f)                                

	i, _ := strconv.ParseInt("123", 0, 64)     // `0` means infer the base from the string.
	println(i)                                 // `64` requires that the result fit in 64 bits.

	d, _ := strconv.ParseInt("0x1b3e", 0, 64)  // `ParseInt` will recognize hex-formatted numbers.
	println(d)

	k, _ := strconv.Atoi("456")                // `Atoi` is a convenienice function for `int` parsing.
	println(k)
	
	_, e := strconv.Atoi("wat")                // Parse functions return an error on bad input.
	fmt.Println(e)
}

/*
$ go run xx-number-parsing.go
1.234
123
6974
456
strconv.ParseInt: parsing "wat": invalid syntax
*/
