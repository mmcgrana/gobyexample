// ## Spawning Processes

package main

import "os/exec"
import "fmt"

func main() {
    cmd := exec.Command("ls", "-a", "-l")
    out, err := cmd.Output()
    if err != nil {
        panic(err)
    }
    fmt.Println("Files:")
    fmt.Print(string(out))
}

// todo: full command lines with bash
// todo: piping in stdin
