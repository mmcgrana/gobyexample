package main

import "strings"
import "fmt"

func Index(strs []string, s string) int {
    for i, str := range strs {
        if s == str {
            return i
        }
    }
    return -1
}

func Include(elems []string, val string) bool {
    return Index(elems, val) >= 0
}

func Any(elems []string, f func(string) bool) bool {
    for _, v := range elems {
        if f(v) {
            return true
        }
    }
    return false
}

func All(elems []string, f func(string) bool) bool {
    for _, v := range elems {
        if !f(v) {
            return false
        }
    }
    return true
}

func Filter(vs []string, f func(string) bool) []string {
    vsf := make([]string, 0)
    for _, v := range vs {
        if f(v) {
            vsf = append(vsf, v)
        }
    }
    return vsf
}

func Map(strs []string, f func(string) string) []string {
    mapped := make([]string, len(strs))
    for i, v := range strs {
        mapped[i] = f(v)
    }
    return mapped
}

func main() {
    var strs = []string{"peach", "apple", "pear", "plum"}

    fmt.Println(Index(strs, "pear"))
    fmt.Println(Index(strs, "grape"))
    fmt.Println()

    fmt.Println(Include(strs, "pear"))
    fmt.Println(Include(strs, "grape"))
    fmt.Println()

    fmt.Println(Any(strs, func(v string) bool {
        return strings.HasPrefix(v, "p")
    }))
    fmt.Println(Any(strs, func(v string) bool {
        return strings.HasPrefix(v, "g")
    }))
    fmt.Println()

    fmt.Println(All(strs, func(v string) bool {
        return strings.Contains(v, "a")
    }))
    fmt.Println(All(strs, func(v string) bool {
        return strings.Contains(v, "p")
    }))
    fmt.Println()

    fmt.Println(Filter(strs, func(v string) bool {
        return strings.Contains(v, "p")
    }))
    fmt.Println()

    fmt.Println(Map(strs, func(s string) string {
        return strings.ToUpper(s)
    }))
    fmt.Println()
}

// todo: note no generics
