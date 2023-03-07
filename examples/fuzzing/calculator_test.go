package main

import (
	"bytes"
	"errors"
	"math/rand"
	"strconv"
	"testing"
)

/*A calculator for performing simple arithmetic operations*/

/*
[]byte was used as its parameters in the Calculator() cause fuzz test  requires []bytes as
 it parameters when testing a function
 then the []bytes are coverted to []float for performing arithmetic operations
*/

func Calculator(numbers []byte) (float64, error) {
	bytesList := bytes.Split(numbers, []byte(","))

	//convert to float
	var floatList []float64
	for _, b := range bytesList {
		f, err := strconv.ParseFloat(string(b), 64)
		if err != nil {
			return 0, err
		}
		floatList = append(floatList, f)
	}

	var result float64
	var operator string

	//performing calculations based on the indices of the values in FloatList
	switch operator {
	case "-":
		if floatList[0] > floatList[1] || floatList[0] < floatList[1] {
			result = floatList[0] - floatList[1]
			return result, nil
		} else if floatList[1] > floatList[0] || floatList[1] < floatList[0] {
			result = floatList[1] - floatList[0]
			return result, nil
		}

	case "+":
		if floatList[0] > floatList[1] || floatList[0] < floatList[1] {
			result = floatList[0] + floatList[1]
			return result, nil
		} else if floatList[1] > floatList[0] || floatList[1] < floatList[0] {
			result = floatList[1] + floatList[0]
			return result, nil
		}

	case "/":
		if floatList[0] == 0 || floatList[1] == 0 {
			return 0, errors.New("Division by 0")
		}
		result = floatList[0] / floatList[1]

	case "*":
		result = floatList[0] * floatList[1]
	}

	return result, nil
}

func FuzzCalculator(f *testing.F) {
	//generate random numbers btwn 0-100
	testcases := []float64{rand.Float64() * 100, rand.Float64() * 100}

	//convert to bytes
	var b []byte
	for _, n := range testcases {
		b = strconv.AppendFloat(b, n, 'f', -1, 64)
		b = append(b, ',')
	}
	f.Add(b) //seed corpus inputs performed

	//Runs the fuzz function for testing
	f.Fuzz(func(t *testing.T, numbers []byte) {
		_, err := Calculator(numbers)
		if err != nil {
			return
		}
		t.Log("Calculations Test Passed")
	})
}
