// ## Sorting by Functions

package main

import (
    "fmt"
    "sort"
)

type Person struct {
    Name string
    Age  int
}

type ByName []Person

func (this ByName) Len() int {
    return len(this)
}
func (this ByName) Less(i, j int) bool {
    return this[i].Name < this[j].Name
}
func (this ByName) Swap(i, j int) {
    this[i], this[j] = this[j], this[i]
}

type ByAge []Person

func (this ByAge) Len() int {
    return len(this)
}
func (this ByAge) Less(i, j int) bool {
    return this[i].Age < this[j].Age
}
func (this ByAge) Swap(i, j int) {
    this[i], this[j] = this[j], this[i]
}

func main() {
    kids := []Person{
        {"Jack", 10},
        {"Jill", 9},
        {"Bob", 12},
    }
    fmt.Println("Original:", kids)

    sort.Sort(ByName(kids))
    fmt.Println("ByName:  ", kids)

    sort.Sort(ByAge(kids))
    fmt.Println("ByAge:   ", kids)
}

// todo: pull from blog post
