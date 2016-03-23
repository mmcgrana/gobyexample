// Go permette l'utilizzo dei <a href="https://it.wikipedia.org/wiki/Metodo_(programmazione)"><em>metodi</em></a>
// definiti su tipi all'interno di uno stesso package.

package main

import "fmt"

type rect struct {
	width, height int
}

// Questo metodo `area` ha un _receiver type_ `*rect`.
func (r *rect) area() int {
	return r.width * r.height
}

// I metodi possono essere definiti o per un puntatore
// o per semplicemente un valore. Ecco un esempio di
// metodo con il _receiver type_ valore.
func (r rect) perim() int {
	return 2*r.width + 2*r.height
}

func main() {
	r := rect{width: 10, height: 5}

	// Qui chiamiamo i 2 metodi definiti per la nostra struct.
	fmt.Println("area: ", r.area())
	fmt.Println("perim:", r.perim())

	// Le conversioni tra puntatori e valori per le
	// chiamate a metodi vengono effettuate
	// automaticamente da Go. Potresti voler usare un
	// puntatore come receiver type per evitare di copiare
	// nelle chiamate al metodo o per permettere al metodo
	// di modificare il valore originale della variabile.
	rp := &r
	fmt.Println("area: ", rp.area())
	fmt.Println("perim:", rp.perim())
}
