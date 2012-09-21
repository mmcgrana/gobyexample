package main

import ("time"; "fmt")

func main() {
	timer1 := time.NewTimer(time.Millisecond * 500)
	<- timer1.C
	fmt.Println("Timer 1 expired")
	stopped1 := timer1.Stop()
	fmt.Println("Timer 2 stopped:", stopped1)

	timer2 := time.NewTimer(time.Second)
	go func() {
		<- timer2.C
		fmt.Println("Timer 2 expired")
	}()
	time.Sleep(time.Millisecond * 500)
	stopped2 := timer2.Stop()
	fmt.Println("Timer 2 stopped:", stopped2)
}
