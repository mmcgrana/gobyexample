package main

import ("time"; "fmt")

func main() {
	start := time.Now()
	time.Sleep(3 * time.Second)
	finish := time.Now()
	fmt.Println(finish.Sub(start))
}
