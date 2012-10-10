// Switch statements allow...

package main

import "fmt"

func main() {
    fmt.Print("Write 3 as ")
    i := 3
    // Some basic commentary on switches.
    switch i {
    case 0:
        fmt.Println("zero")
    case 1:
        fmt.Println("one")
    case 2:
        fmt.Println("two")
    case 3:
        fmt.Println("three")
    case 4:
        fmt.Println("four")
    case 5:
        fmt.Println("five")
    // The `default` branch is optional in a `switch`.
    default:
        fmt.Println("???")
    }
}

// todo: more complex / non-constant switch?
