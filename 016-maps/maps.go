// ## Maps

package main

import "fmt"

func main() {
	x := make(map[string]int)
	x["this"] = 7
	x["that"] = 13
	fmt.Println(x)
	fmt.Println(x["this"])
	fmt.Println(x["missing"])
	fmt.Println(len(x))
	delete(x, "that")
	fmt.Println(x["that"])
	fmt.Println(len(x))
	_, present := x["that"]
	fmt.Println(present)
}
