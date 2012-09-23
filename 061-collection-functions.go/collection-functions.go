// ## Collection Functions

package main            

import "strings"
import "fmt"

func Index(elems []string, val string) int {
	for i, v := range elems {
		if v == val {
			return i
		}
	}
	return -1
}

func Include(elems []string, val string) bool {
	return Index(elems, val) >= 0
}

func Any(elems []string, f func(string)bool) bool {
	for _, v := range elems {
		if f(v) {
			return true
		}
	}
	return false
}

func All(elems []string, f func(string)bool) bool {
	for _, v := range elems {
		if !f(v) {
			return false
		}
	}
	return true
}

func Filter(elems []string, f func(string)bool) []string {
	filtered := []string{}
	for _, v := range elems {
		if f(v) {
			filtered = append(filtered, v)
		}
	}
	return filtered
}

func Map(elems []string, f func(string)string) []string {
	mapped := make([]string, len(elems))
	for i, v := range elems {
		mapped[i] = f(v)
	}
	return mapped
}

func main() {
	var elems = []string{"peach", "apple", "pear", "banana"}

	fmt.Println(Index(elems, "pear"))
	fmt.Println(Index(elems, "grape"))
	fmt.Println()

	fmt.Println(Include(elems, "pear"))
	fmt.Println(Include(elems, "grape"))
	fmt.Println()

	fmt.Println(Any(elems, func(v string) bool {
		return strings.HasPrefix(v, "p")
	}))
	fmt.Println(Any(elems, func(v string) bool {
		return strings.HasPrefix(v, "g")
	}))
	fmt.Println()

	fmt.Println(All(elems, func(v string) bool {
		return strings.Contains(v, "a")
	}))
	fmt.Println(All(elems, func(v string) bool {
		return strings.Contains(v, "p")
	}))
	fmt.Println()

	fmt.Println(Filter(elems, func(v string) bool {
		return strings.Contains(v, "p")
	}))
	fmt.Println()
	
	fmt.Println(Map(elems, func(s string) string {
		return strings.ToUpper(s)
	}))
	fmt.Println()
}

// todo: generics
