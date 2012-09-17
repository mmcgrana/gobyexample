package main

import ("math/rand"; "fmt")

func main() {
	fmt.Println(rand.Intn(100))
	fmt.Println(rand.Intn(100))

	r1 := rand.New(rand.NewSource(int64(1337)))
	fmt.Println(r1.Intn(100))
	fmt.Println(r1.Intn(100))
	r2 := rand.New(rand.NewSource(int64(1337)))
	fmt.Println(r2.Intn(100))
	fmt.Println(r2.Intn(100))
}
