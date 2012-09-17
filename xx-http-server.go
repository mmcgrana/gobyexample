package main

import ("net/http"; "io")

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
    io.WriteString(res, "Hello From HTTP\n")
}

func main() {
    http.HandleFunc("/", hello)
    http.ListenAndServe(":5000", nil)
}
