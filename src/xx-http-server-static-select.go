package main

import ("net/http"; "fmt"; "code.google.com/p/gorilla/mux")

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	fmt.Fprintln(res, "Hello")
}

func static(res http.ResponseWriter, req *http.Request) {
	res.Header().Set("Content-Type", "text/plain")
	fmt.Fprintln(res, "Static")
}

func notFound(res http.ResponseWriter, req *http.Request) {
	res.Header().Set("Content-Type", "text/plain")
	res.WriteHeader(404)
	fmt.Fprintln(res, "Not found: /" + mux.Vars(req)["path"])
}

func main() {
	r := mux.NewRouter()
	r.HandleFunc("/", hello)
	r.HandleFunc("/favicon.ico", static)
	r.HandleFunc("/{path:.*}", notFound)
	http.ListenAndServe(":5000", r)
}

// == running
// $ go get code.google.com/p/gorilla/mux
// $ go run xx-http-server-static-select.go
//
// $ curl -i http://127.0.0.1:5000/
// $ curl -i http://127.0.0.1:5000/favicon.ico
// $ curl -i http://127.0.0.1:5000/wat
// $ curl -i http://127.0.0.1:5000/wat/wat
