// Modificare il flusso di controllo con `if` ed `else` in Go
// è semplice e ricalca la classica sintassi vista in altri linguaggi.

package main

import "fmt"

func main() {

    // Questo è un esempio base
    if 7%2 == 0 {
        fmt.Println("7 è pari")
    } else {
        fmt.Println("7 è dispari")
    }

    // È possibile avere un comando `if` senza il ramo `else`
    if 8%4 == 0 {
        fmt.Println("8 è divisibile per 4")
    }

    // Un comando può precedere il test del comando `if`.
    // Qualsiasi variabile dichiarata in questo comando
    // è visibile all'interno di tutti i rami del comando `if`
    if num := 9; num < 0 {
        fmt.Println(num, "è negativo")
    } else if num < 10 {
        fmt.Println(num, "ha una cifra")
    } else {
        fmt.Println(num, "ha più di una cifra")
    }
}

// Nota che non sono necessarie le parentesi intorno alle condizioni
// del comando `if` in Go, ma le parentesi graffe sono necessarie.
