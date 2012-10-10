package main

import "net/http"

func handler(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
    res.Write([]byte("Hello from HTTPS\n"))
}

func main() {
    http.HandleFunc("/", handler)
    err := http.ListenAndServeTLS(":5000",
        "/tmp/server.crt", "/tmp/server.key", nil)
    if err != nil {
        panic(err)
    }
}
