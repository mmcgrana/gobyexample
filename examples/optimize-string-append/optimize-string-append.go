// This example is inspired from this [benchmark.](http://herman.asia/efficient-string-concatenation-in-go)
// If string join is your program's bottleneck. It is worth to rewrite your code.
package main

import (
	"fmt"
	"os"
	"time"
)

func main() {
	const testCnt = 100
	const concatCnt = 1000
	s1 := "abcdefghijkl"
	expectSize := len(s1) * concatCnt

	// We use normal '+' operater and require 1779.8ms
	t0 := time.Now()
	for i := 0; i < testCnt; i++ {
		var out string
		for j := 0; j < concatCnt; j++ {
			out += s1
		}
		if len(out) != expectSize {
			fmt.Println("Err: invalid length", len(out))
			os.Exit(1)
		}
	}
	d := time.Now().Sub(t0)
	fmt.Println("Normal +:", d)

	// We use byte array, and convert it to string  at the last step.
	// The required time reduce to 53.7ms (about 1/33 of original time)
	t0 = time.Now()
	for i := 0; i < testCnt; i++ {
		var s []byte
		for j := 0; j < concatCnt; j++ {
			s = append(s, s1...)
		}
		out := string(s[:])
		if len(out) != expectSize {
			fmt.Println("Err: invalid length", len(out))
			os.Exit(1)
		}
	}
	d = time.Now().Sub(t0)
	fmt.Println("append by []byte:", d)

	// We use pre-allocated byte array, and convert it to string
	// at the last step.
	// The required time reduce to 19.6ms (about 1/90.8)
	t0 = time.Now()
	for i := 0; i < testCnt; i++ {
		s := make([]byte, 0, expectSize)
		for j := 0; j < concatCnt; j++ {
			s = append(s, s1...)
		}
		out := string(s[:])
		if len(out) != expectSize {
			fmt.Println("Err: invalid length", len(out))
			os.Exit(1)
		}
	}
	d = time.Now().Sub(t0)
	fmt.Println("append by pre-allocated []byte:", d)
}
