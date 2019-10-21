package main

import (
	"encoding/csv"
	"fmt"
	"log"
	"os"
)

func main() {
	resp, err := os.Open("example.csv")
	if err != nil {
		log.Fatalln("Couldn't open the csv file", err)
	}
	r, err := csv.NewReader(resp).ReadAll()
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(r)

}
