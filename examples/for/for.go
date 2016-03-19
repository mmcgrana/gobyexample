// `for` è l'unico costrutto per eseguire cicli
// in Go. Qui vengono presentati tre tipi di cicli
// `for`.

package main

import "fmt"

func main() {

    // Il ciclo più semplice, con una singola condizione.
    // (simile al while degli altri linguaggi)
    i := 1
    for i <= 3 {
        fmt.Println(i)
        i = i + 1
    }

    // Un classico ciclo `for` inizializzazione/test/incremento.
    for j := 7; j <= 9; j++ {
        fmt.Println(j)
    }

    // Un `for` senza condizioni si ripeterà sempre finché
    // non esci dal ciclo con un `break` oppure fai
    // un `return` per la funzione che lo racchiude.
    for {
        fmt.Println("loop")
        break
    }
}
