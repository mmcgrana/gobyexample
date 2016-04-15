// In Go, le _variabli_ sono dichiarate esplicitamente e
// sono usate dal compilatore, ad esempio, per contrillare
// la correttezza dei tipi di valori nelle invocazioni
// delle funzioni.

package main

import "fmt"

func main() {

    // `var` dichiara una o più variabili.
    var a string = "initial"
    fmt.Println(a)

    // Puoi dichiarare più variabili in un colpo solo.
    var b, c int = 1, 2
    fmt.Println(b, c)

    // Go dedurrà il tipo delle variabili inizializzate.
    var d = true
    fmt.Println(d)

    // Variabili dichiarate senza una inizializzazione
    // corrispondente sono _zero-valued_. Ad esempio, lo
    // zero-value di un `int` è `0`.
    var e int
    fmt.Println(e)

    // La sintassi `:=` è una abbreviazione per dichiarare
    // ed inizializzare una variabile, in questo caso è
    // l'abbreviazione di `var f string = "short"`.
    f := "short"
    fmt.Println(f)
}
