package main

import ("fmt"; "strings")

func p(o interface{}) {
	fmt.Println(o)
}

func main() {
    p(strings.Contains("test", "es"))
    p(strings.Count("test", "t"))
    p(strings.HasPrefix("test", "te"))
    p(strings.HasSuffix("test", "st"))
	p(strings.Index("test", "e"))
    p(strings.Join([]string{"a","b"}, "-"))
    p(strings.Repeat("a", 5))
    p(strings.Replace("aaaa", "a", "b", 2))
    p(strings.Split("a-b-c-d-e", "-"))
    p(strings.ToLower("TEST"))
    p(strings.ToUpper("test"))
}
