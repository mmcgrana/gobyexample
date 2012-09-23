// ## HTTP Server Static Dynamic

package main

import "net/http"

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	res.Write([]byte("Hello From HTTP\n"))
}

func main() {
	helloHandler  := http.HandlerFunc(hello)
	staticHandler := http.StripPrefix("/static/", http.FileServer(http.Dir("./")))
	http.Handle("/hello",   helloHandler)
	http.Handle("/static/", staticHandler)
	http.ListenAndServe(":5000", nil)
}

// todo: try to get dynamic at root
// todo: try to get static at root
// todo: favicon
