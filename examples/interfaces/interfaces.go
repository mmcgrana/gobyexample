// Le _Interfacce_ in Go sono collezioni di
// [firme (signature)](https://it.wikipedia.org/wiki/Firma_(programmazione)) di metodi.

package main

import "fmt"
import "math"

// Questa è una semplice interfaccia per le forme geometriche.
type geometry interface {
    area() float64
    perim() float64
}

// Per il nostro esempio implementeremo questa inferfaccia
// per le struct `rect` e `circle`.
type rect struct {
    width, height float64
}
type circle struct {
    radius float64
}

// Per implementare un interfaccia in Go è sufficiente implementare
// tutti i metodi dell'interfaccia.
// Qui stiamo implementando l'interfaccia `geometry` per il tipo `rect`.
func (r rect) area() float64 {
    return r.width * r.height
}
func (r rect) perim() float64 {
    return 2*r.width + 2*r.height
}

// Qui invece l'implementazione per `circle`.
func (c circle) area() float64 {
    return math.Pi * c.radius * c.radius
}
func (c circle) perim() float64 {
    return 2 * math.Pi * c.radius
}

// Se una variabile ha il tipo di un'interfaccia possiamo invocare i metodi
// dell'interfaccia stessa. Qui è presente una funzione `measure` generica
// che funzionerà su ogni variabile di tipo `geometry`.
func measure(g geometry) {
    fmt.Println(g)
    fmt.Println(g.area())
    fmt.Println(g.perim())
}

func main() {
    r := rect{width: 3, height: 4}
    c := circle{radius: 5}

    // Sia `circle` che `rect` implementano l'interfaccia
    // `geometry`, possiamo quindi passare alla funzione
    // `measure` istance di queste due struct.
    measure(r)
    measure(c)
}
