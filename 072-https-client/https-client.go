// ## HTTPS Client

package main

import "net/http"
import "crypto/tls"
import "io/ioutil"
import "fmt"

func main() {
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	client := &http.Client{Transport: tr}
	resp, err := client.Get("https://127.0.0.1:5000/")
	if err != nil { panic(err) }
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)
	fmt.Print(string(body))
}
