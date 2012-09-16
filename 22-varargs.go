package main

import "fmt"

func add(nums ...int) int {
	total := 0
	for _, num := range nums {
		total += num
	}
	return total
}

func main() {
	fmt.Println(add(1, 2, 3, 4))
}
