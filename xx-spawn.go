package main

import ("os/exec"; "fmt")

func main() {
	cmd := exec.Command("ls", "-a" "-l", "-h")
	out, err := cmd.Output()
	if err != nil {
		panic(err)
	}
	fmt.Println("====== Output")
	fmt.Print(string(out))
}
