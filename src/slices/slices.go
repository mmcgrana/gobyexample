package main

import "fmt"

func main() {
    slice1 := []int{1, 2, 3}
    slice2 := append(slice1, 4, 5)
    fmt.Println(slice1)
    fmt.Println(slice2)
    slice3 := make([]int, 2)
    copy(slice3, slice1)
    fmt.Println(slice1)
    fmt.Println(slice3)
}
