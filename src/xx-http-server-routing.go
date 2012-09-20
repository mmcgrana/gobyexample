package main

import ("net/http"; "github.com/bmizerany/pat"; "fmt")

func hello(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintln(w, "hello " + req.URL.Query().Get(":name"))
}

func main() {
	p := pat.New()
	p.Get("/hello/:name", http.HandlerFunc(hello))
	http.ListenAndServe(":5000", p)
}

// == running
// $ go get github.com/bmizerany/pat
// $ go run xx-http-server-routing.go
//
// $ curl -i http://127.0.0.1:5000/hello/gopher
