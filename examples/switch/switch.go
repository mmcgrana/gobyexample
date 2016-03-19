// Gli _switch_ esprimono condizionali attraverso più
// rami.

package main

import "fmt"
import "time"

func main() {

    // Ecco uno switch semplice.
    i := 2
    fmt.Print(i, " in lettere è ")
    switch i {
    case 1:
        fmt.Println("uno")
    case 2:
        fmt.Println("due")
    case 3:
        fmt.Println("tre")
    }

    // Puoi utilizzare le virgole per dividere più
    // espressioni nella stessa dichiarazione `case`.
    // In questo esempio utilizziamo anche il caso
    // opzionale `default`, che viene eseguito nel
    // caso l'espressione non possa essere valutata
    // in nessuno dei rami precedenti.
    switch time.Now().Weekday() {
    case time.Saturday, time.Sunday:
        fmt.Println("siamo nel fine settimana")
    default:
        fmt.Println("oggi è un giorno feriale")
    }

    // Uno `switch` senza espressione è un metodo
    // alternativo per esprimere la logica degli if/else.
    // Qui vediamo anche come le espressioni dei `case`
    // possono anche non essere costanti.
    t := time.Now()
    switch {
    case t.Hour() < 12:
        fmt.Println("non è ancora passato mezzogiorno")
    default:
        fmt.Println("è passato mezzogiorno")
    }

    // Nel caso volessimo fare cose differenti per una
    // variabile di cui non conosciamo il tipo
    // (ad esempio, una variabile `interface{}` che vedremo
    // più avanti), possiamo utilizzare un `type switch`.
    // In questo caso, stiamo prima convertendo la variabile
    // v in una `interface{}`, dopo stiamo usando `.(type)`
    // che segnala di usare il type switch.
    v := 3
    switch interface{}(v).(type) {
    case string:
        fmt.Println("v è di tipo `string`")
    case int:
        fmt.Println("v è di tipo `int`")
    default:
        fmt.Println("v è di un altro tipo ancora")
    }
}
