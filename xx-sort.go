package main

import ("fmt"; "sort")

func main() {
	strs := []string{"foo", "bar", "bat"}
	fmt.Println(strs)
	fmt.Println()

	sort.Strings(strs)
	fmt.Println(strs)
}
