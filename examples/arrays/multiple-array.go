package main

import "fmt"

func main() {
	//multiple array
	// arrayName:= [row][coloumn]arrayType {{00,01},{10,11},{20,21}}

	array := [3][2]int{{11, 12}, {21, 22}, {31, 32}}
	fmt.Println("all of array : ", array)

	array2 := [2][3]int{{1, 2, 1}, {1, 1, 1}}
	fmt.Println(array2)

	for row := 0; row < 3; row++ {
		for coloumn := 0; coloumn < 2; coloumn++ {
			fmt.Print(array[row][coloumn], "  ")
		}
		fmt.Println()
	}
}
