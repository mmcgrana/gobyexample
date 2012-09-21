package main

import ("time"; "fmt")

func main() {
	start := time.Now()
	time.Sleep(3 * time.Second)
	fmt.Println(time.Since(start))
}
