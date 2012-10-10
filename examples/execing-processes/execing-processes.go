// In the previous example we looked at spawning external
// process. We do this when we need the functionality
// of another process accessable to a running Go process.
// In other cases we may just want to completely replace
// the current Go process with another process. To do
// this we'll use Go's implementation of the `exec`.

// In this example we'll exec an `ls` command.
package main

import "syscall"
import "os"
import "os/exec"

func main() {
    // We'll need an absolute path to the binary we'd
    // like to execute. In this case we'll get the path
    // for `ls`, probably `/bin/ls`.
    binary, lookErr := exec.LookPath("ls")
    if lookErr != nil {
        panic(lookErr)
    }

    // Exec requires arguments in slice form (as
    // apposed to one big string). Here we'll give `ls`
    // a few arguments
    args := []string{"-a", "-l", "-h"}

    // We'll give the command we execute our current
    // environment.
    env := os.Environ()

    // The actual exec call. If this call is succesful,
    // the execution of our process will end here and it
    // will be replaced by the `/bin/ls -a -l -h` process.
    // If there is an error we'll get a return value.
    execErr := syscall.Exec(binary, args, env)
    if execErr != nil {
        panic(execErr)
    }
}
