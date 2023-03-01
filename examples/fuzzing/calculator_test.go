package main

/* Fuzz testing or fuzzing is an automated software testing method
that injects invalid, malformed, or unexpected inputs into a system
to reveal software defects and vulnerabilities.
A fuzzing tool injects these inputs into the system and then monitors for exceptions such as crashes or information leakage
*/

//A simple way of using fuzzing to test a program using a claculator instance
import (
	"errors"
	"fmt"
	"testing"
)

/*A calculator for performing simple arithmetic operations*/
func calculator(num1, num2 float64, operator string) (float64, error) {
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

/*Accepts inputs, calls the calculator function and executes inputs and return results*/
func InputCalculator(data float64) float64 {
	inputcases := []struct {
		input1   float64
		input2   float64
		operator string
		output   float64
	}{
		{1, 3, "+", 4},
		{4, 6, "-", -2},
		{0, 0, "/", 0},
		{9, 2, "*", 18},
		{1.2, 2.4, "*", 2.88},
		{2, 6, "+", 9},
	}
	success := 1
	for _, input := range inputcases {
		result, _ := calculator(input.input1, input.input2, input.operator)
		if result != input.output {
			fmt.Errorf("Calculator(%f,%f,%s)=%f; expected %f", input.input1, input.input2,
				input.operator, result, input.output)
		}
		return result
	}
	return float64(success)

}

// testing the Calculator inputs function
func FuzzTestInput(f *testing.F) {
	f.Fuzz(func(t *testing.T, data float64) {
		_ = InputCalculator(data)
	})

}

// Tests the calculator function
func FuzzTestCalculator(f *testing.F) {
	f.Fuzz(func(t *testing.T, num1, num2 float64, operator string) {
		_, _ = calculator(num1, num2, operator)
	})
}
