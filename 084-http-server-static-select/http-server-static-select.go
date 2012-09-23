// ## HTTP Server Static Select

package main

import "code.google.com/p/gorilla/mux"
import "net/http"
import "fmt"

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	fmt.Fprintln(res, "Hello")
}

func static(res http.ResponseWriter, req *http.Request) {
	http.ServeFile(res, req, "./01-hello.go")
}

func notFound(res http.ResponseWriter, req *http.Request) {
	res.Header().Set("Content-Type", "text/plain")
	res.WriteHeader(404)
	fmt.Fprintln(res, "Not found: /" + mux.Vars(req)["path"])
}

func main() {
	r := mux.NewRouter()
	r.HandleFunc("/", hello)
	r.HandleFunc("/01-hello.go", static)
	r.HandleFunc("/{path:.*}", notFound)
	http.ListenAndServe(":5000", r)
}
