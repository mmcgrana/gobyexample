package main

import ("os"; "fmt"; "strings")

func main() {
	for _, e := range os.Environ() {
		pair := strings.Split(e, "=")
		fmt.Println(pair[0], pair[1])
	}
	fmt.Println()

	fmt.Println(os.Getenv("PWD"))
	fmt.Println()

	os.Setenv("FOO", "bar")
	fmt.Println(os.Getenv("FOO"))
}

// == todo
// ensure pattern
// link to 12 factor
