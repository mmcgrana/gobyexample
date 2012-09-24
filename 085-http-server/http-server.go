// ## HTTP Server

package main

import "net/http"

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	res.Write([]byte("Hello From HTTP\n"))
}

func main() {
    http.HandleFunc("/", hello)
    http.ListenAndServe(":5000", nil)
}
