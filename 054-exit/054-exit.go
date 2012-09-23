package main            // Use `os.Exit` to immediatly exit with a given
                        // status.
import  "os"

func main() {
    defer println("!")  // This `println` will never be reached.
	os.Exit(3)  
}

/*
$ go run exit.go        // If you run `exit.go` using `go run`, the exit
exit status 3           // will be picked up by `go` and printed.
                        
$ go build exit.go      // By building and executing a binary you can see
$ ./exit                // the status in the terminal
$ echo $?
3
*/

// == todo
// discuss building before getting here
