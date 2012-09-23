// Exit

package main            

// Use `os.Exit` to immediatly exit with a given
// status.
import  "os"

func main() {
    // This `println` will never be reached because the
    // exit is immediate.
    defer println("!")  
	os.Exit(3)  
}

// todo: discuss building before getting here
