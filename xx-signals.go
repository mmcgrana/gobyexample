package main

import ("fmt"; "os"; "os/signal"; "syscall")

func main() {
	c := make(chan os.Signal, 1)
	d := make(chan bool, 1)

	signal.Notify(c, syscall.SIGINT)
	go func(){
		sig := <- c
		fmt.Println()
		fmt.Println(sig)
		d <- true
	}()
	fmt.Println("Awaiting signal")
	<- d
}
