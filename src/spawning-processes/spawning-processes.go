// Sometimes our Go programs need to spawn other, non-Go
// processes. For example, the syntax highlighting in this
// book is implementing by spawning a [`pygmentize`]()
// process from a Go program. Let's look at a few
// examples of spawning processes from Go.

package main

import "os/exec"
import "fmt"

func main() {
    // todo: explain
    dateCmd := exec.Command("date")
    dateOut, dateErr := dateCmd.Output()
    if dateErr != nil {
        panic(dateErr)
    }
    fmt.Println("> date")
    fmt.Println(string(dateOut))

    // todo: piping in stdin

    // Note that when spawning commands we need to
    // provide an explicit command and argument array,
    // vs. being able to just pass in one command line.
    // If you want to be able to just spawn a full
    // command, you can use `bash`'s `-c` option:
    lsCmd := exec.Command("bash", "-c", "ls -a -l -h")
    lsOut, lsErr := lsCmd.Output()
    if lsErr != nil {
        panic(lsErr)
    }
    fmt.Println("> ls -a -l -h")
    fmt.Println(string(lsOut))
}
