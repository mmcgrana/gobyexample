package main

import ("time"; "fmt")

func main() {
	now := time.Now()
	secs := now.Unix()
	nanos := now.UnixNano()
	millis := nanos / 1000000
	fmt.Println("Seconds since epoch:", secs)
	fmt.Println("Millis  since epoch:", millis)
	fmt.Println("Nanos   since epoch:", nanos)
}
