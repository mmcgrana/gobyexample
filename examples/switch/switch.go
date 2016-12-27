// _Switch statements_ express conditionals across many
// branches.

package main

import "fmt"
import "time"

func main() {

    // Here's a basic `switch`.
    i := 2
    fmt.Print("write ", i, " as ")
    switch i {
    case 1:
        fmt.Println("one")
    case 2:
        fmt.Println("two")
    case 3:
        fmt.Println("three")
    }

    // You can use commas to separate multiple expressions
    // in the same `case` statement. We use the optional
    // `default` case in this example as well.
    switch time.Now().Weekday() {
    case time.Saturday, time.Sunday:
        fmt.Println("it's the weekend")
    default:
        fmt.Println("it's a weekday")
    }

    // `switch` without an expression is an alternate way
    // to express if/else logic. Here we also show how the
    // `case` expressions can be non-constants.
    t := time.Now()
    switch {
    case t.Hour() < 12:
        fmt.Println("it's before noon")
    default:
        fmt.Println("it's after noon")
    }

    // A type `switch` compares types instead of values.  You
    // can use this to discover the the type of an interface
    // value.  In this example, the variable `t` will have the
    // type corresponding to its clause.
    whatAmI := func(i interface{}) string {
        switch t := i.(type) {
        case bool:
            return "I am a bool"
        case int:
            return "I am an int"
        default:
            return fmt.Sprintf("Can't handle type %T", t)
        }
    }
    fmt.Println(whatAmI(1))
    fmt.Println(whatAmI(true))
    fmt.Println(whatAmI("hey"))
}
