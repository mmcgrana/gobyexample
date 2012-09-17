package main

import "fmt"

func main() {
	arr := []byte("some bytes")
	str := string([]byte{'a', ' ', 's', 't', 'r', 'i', 'n', 'g'})
	fmt.Println(arr)
	fmt.Println(str)
}
