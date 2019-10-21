// In this example, we set timeout for clients

package main

import (
	"fmt"
	"net/http"
	"time"
)

func sendClient(url string) {

	client := http.Client{
		Timeout: time.Duration(10 * time.Second),
	}

	resp, err := client.Get(url)

	if err != nil {
		fmt.Println("ERROR: Failed to Client \"" + url + "\"")
		return
	}

	//	fmt.Println(resp)
	defer resp.Body.Close()

}

func main() {

	url := "https://gobyexample.com/"
	sendClient(url)

}
