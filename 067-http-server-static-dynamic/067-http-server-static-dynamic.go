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

// == running
// $ cd src
// $ go run xx-http-server-static-dynamic.go
// 
// $ curl http://127.0.0.1:5000/hello
// $ curl http://127.0.0.1:5000/static
// $ curl http://127.0.0.1:5000/static/01-hello.go

// == todo
// try to get dynamic at root
// try to get static at root
// favicon
