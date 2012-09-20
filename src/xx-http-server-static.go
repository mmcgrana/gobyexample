package main

import "net/http"

func main() {
	handler := http.FileServer(http.Dir("./"))
	http.ListenAndServe(":5000", handler)
}

// == running
// $ cd src
// $ go run xx-http-server-static.go
// 
// $ curl http://127.0.0.1:5000/
// $ curl http://127.0.0.1:5000/xx-http-server-static.go
// $ curl http://127.0.0.1:5000/missing

// == todo
// serving only a subpath as statically
// index pages
// disable listing
// custom 404 handling
