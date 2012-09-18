package main

import ("fmt"; "flag")

func main() {
    maxp := flag.Int("repeat", 3, "number of times to repeat arguments")
    flag.Parse()
	for i := 0; i < *maxp; i++ {
		for _, arg := range flag.Args() {
			fmt.Println(arg)
		}
	}
}
