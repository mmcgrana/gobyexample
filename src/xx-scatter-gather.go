package main

import ("sync"; "time"; "math/rand"; "fmt")

func main() {
	times := new([20]int)
	wait := new(sync.WaitGroup)
	for i := 0; i < 20; i++ {
		n := i
	  	wait.Add(1)
	  	go func() {
	  	  	opTime := rand.Intn(2000)
	  	  	time.Sleep(time.Duration(opTime) * time.Millisecond)
	  	  	fmt.Println(n)
	  	  	times[n] = opTime
	  	  	wait.Done()
	  	}()
	}
	wait.Wait()
	fmt.Println(*times)
}
