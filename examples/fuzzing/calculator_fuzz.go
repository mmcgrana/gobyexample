package main

import (
	"errors"
	"fmt"
	"math/rand"
	"strconv"
	"strings"
)

func Calculator(num1, num2 float64, operator string) (float64, error) {
	var result float64

	switch operator {
	case "-":
		result = num1 - num2

	case "+":
		result = num1 + num2

	case "/":
		if num2 == 0 || num1 == 0 {
			return 0, errors.New("Division by 0")
		}
	case "*":
		result = num1 * num2
	}

	return result, nil
}

/*
The go-fuzz testing tools operates on raw binary data
Every input are in []bytes which are coverted to strings and
The string is then split into three parts (two numbers and an operator) using strings.
Split, and the numbers are parsed using strconv.ParseFloat. Finally,
the Calculator function is called with the parsed numbers and operator.
*/
func Fuzz(data []byte) float64 {
	//converts input data into string
	inputs := strings.Split(string(data), ",")
	//parse input numbers and operators
	num1, err := strconv.ParseFloat(inputs[0], 64)
	if err != nil {
		return 0

	}
	num2, err := strconv.ParseFloat(inputs[1], 64)
	if err != nil {
		return 0
	}
	operators := inputs[2]
	_, err = Calculator(num1, num2, operators)
	if err != nil {
		if err.Error() != "Division by 0" {
			return 0
		}
	}
	//returns a success
	return 1
}

func main() {
	// Seed the random number generator
	rand.Seed(1234)

	// Generate random input data
	data := make([]byte, 64)
	rand.Read(data)

	result, err := Calculator(16, 18, "*")
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(result)

	// Fuzz the Calculator function with the generated input data
	Fuzz(data)
}
