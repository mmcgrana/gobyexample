package main

import "net/http"
import "io/ioutil"
import "fmt"

func main() {
    resp, err := http.Get("http://127.0.0.1:5000/")
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)
    fmt.Print(string(body))
}
