package main

import ("fmt"; "flag")

func main() {
    maxp := flag.Int("repeat", 3, "time to repeat args")
    flag.Parse()
	for i := 0; i < *maxp; i++ {
		for _, arg := range flag.Args() {
			fmt.Println(arg)
		}
	}
}

// todoo
// multiple flags
// trailing args
// arg escaping
// help text and usage errors
