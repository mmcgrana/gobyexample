// In Go è possibile utilizzare valori di svariati tipi
// fra i quali anche string, integer, boolean
// float, etc. Vediamo insieme qualche esempio
// basilare su come usare questi tipi.

package main

import "fmt"

func main() {

    // String, che possono essere concatenate con `+`.
    fmt.Println("go" + "lang")

    // Integer e float.
    fmt.Println("1+1 =", 1+1)
    fmt.Println("7.0/3.0 =", 7.0/3.0)

    // Boolean, con i classici operatori booleani
    // AND, OR e NOT.
    fmt.Println(true && false)
    fmt.Println(true || false)
    fmt.Println(!true)
}
