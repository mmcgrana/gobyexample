// In Go le _structs_ sono collezioni di field (campi) a cui è associato
// un tipo. Sono utili per raccogliere insieme dati in modo
// da formare dei record

package main

import "fmt"

// Questa struct `person` possiede due campi,
// rispettivamente `name` ed `age`.
type person struct {
    name string
    age  int
}

func main() {

    // Con questa sintassi si crea una nuova struct.
    fmt.Println(person{"Nicola", 20})

    // Puoi indicare il nome del campo quando crei una struct.
    fmt.Println(person{name: "Luigi", age: 30})

    // I field non indicati verrano inizializzati con
    // il loro zero-value.
    fmt.Println(person{name: "Alessandro"})

    // Inserire un `&` a prefisso della dichiarazione permetterà
    // di ottenere un puntatore alla struct
    fmt.Println(&person{name: "Luca", age: 40})

    // Puoi accedere ai campi della struct con
    // l'operatore `.` (punto).
    s := person{name: "Mario", age: 50}
    fmt.Println(s.name)

    // Puoi utilizzare il punto anche per i puntatori a struct.
    // Il puntatore verrà dereferenziato automaticamente.
    sp := &s
    fmt.Println(sp.age)

    // Le struct sono mutabili.
    sp.age = 51
    fmt.Println(sp.age)
}
