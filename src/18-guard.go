package main

import "fmt"

func main() {
	x := make(map[string]string)
	x["here"] = "yep"
	if name, ok := x["not here"]; ok {    
	    fmt.Println(name)
	} else {
		fmt.Println("missing")
	}
	if name, ok := x["here"]; ok {    
	    fmt.Println(name)
	}
}
