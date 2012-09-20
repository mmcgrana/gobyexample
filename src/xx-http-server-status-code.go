package main

import ("net/http"; "fmt")

func hello(res http.ResponseWriter, req *http.Request) {
	res.Header().Set("Content-Type", "text/plain")
	if req.URL.Path == "/protected" {
		res.WriteHeader(401)
		fmt.Fprintln(res, "Not allowed")
	} else {
		fmt.Fprintln(res, "Hello")
	}	
}

func main() {
    http.HandleFunc("/", hello)
    http.ListenAndServe(":5000", nil)
}

// running
// $ go run xx-http-server-status-code.go
//
// $ curl -i http://127.0.0.1:5000/
// $ curl -i http://127.0.0.1:5000/protected
