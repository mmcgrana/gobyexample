package main

import ("syscall"; "os"; "os/exec")

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
