package main

import "fmt"

type Circle struct {
	x, y, r int
}

func main() {
	c := Circle{x: 1, y: 2, r: 5}
	fmt.Println(c.x, c.y, c.r)
	c.x = 10
	c.y = 5
	fmt.Println(c.x, c.y, c.r)
}
