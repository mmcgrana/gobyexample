package main

import "fmt"
import "math"

type Circle struct {
    x, y, r float64
}

func (c *Circle) area() float64 {
    return math.Pi * c.r * c.r
}

type Rectangle struct {
    x1, y1, x2, y2 float64
}

func distance(x1, y1, x2, y2 float64) float64 {
    a := x2 - x1
    b := y2 - y1
    return math.Sqrt(a*a + b*b)
}

func (r *Rectangle) area() float64 {
    l := distance(r.x1, r.y1, r.x1, r.y2)
    w := distance(r.x1, r.y1, r.x2, r.y1)
    return l * w
}

func main() {
    circle := Circle{x: 0, y: 3, r: 5}
    fmt.Println(circle.area())
    rectangle := Rectangle{x1: 3, x2: 10, y1: 5, y2: 7}
    fmt.Println(rectangle.area())
}

// todo: pointer vs value receivers
