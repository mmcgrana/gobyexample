// ## Exec'ing Processes

package main

import "syscall"
import "os"
import "os/exec"

func main() {
	binary, lookErr := exec.LookPath("ls")
	if lookErr != nil {
		panic(lookErr)
	}
	execErr := syscall.Exec(binary, []string{"-a", "-l", "-h"}, os.Environ())
	if execErr != nil {
		panic(execErr)
	}
}

// todo: note lack of fork
