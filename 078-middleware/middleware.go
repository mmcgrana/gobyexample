// ## Middleware

package main

import "net/http"
import "fmt"

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	fmt.Fprintln(res, "Hello wrapped world")
}

func wrapMiddleware(f http.HandlerFunc) http.HandlerFunc {
	return func(res http.ResponseWriter, req *http.Request) {
		fmt.Println("before")
		f(res, req)
		fmt.Println("after")		
	}
}

func main() {
	handler := wrapMiddleware(hello)
    http.HandleFunc("/", handler)
    http.ListenAndServe(":5000", nil)
}
