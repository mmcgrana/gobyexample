package main

import "net/http"
import "crypto/tls"
import "io/ioutil"
import "fmt"

func main() {
    conf := &tls.Config{InsecureSkipVerify: true}
    trans := &http.Transport{TLSClientConfig: conf}
    client := &http.Client{Transport: trans}
    resp, err := client.Get("https://127.0.0.1:5000/")
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()
    body, _ := ioutil.ReadAll(resp.Body)
    fmt.Print(string(body))
}
