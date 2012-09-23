package main

import "fmt"

func avg(vals []float64) float64 {
	total := 0.0
	for _, val := range vals {
		total += val
	}
	return total / float64(len(vals))
}

func main() {
	input := []float64{98,93,77,82,83}
	fmt.Println(input)
	output := avg(input)
	fmt.Println(output)
}
