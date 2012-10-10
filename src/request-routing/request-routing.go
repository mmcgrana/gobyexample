package main

import "github.com/bmizerany/pat"
import "net/http"
import "fmt"

func hello(w http.ResponseWriter, req *http.Request) {
    fmt.Fprintln(w, "hello "+req.URL.Query().Get(":name"))
}

func main() {
    p := pat.New()
    p.Get("/hello/:name", http.HandlerFunc(hello))
    http.ListenAndServe(":5000", p)
}

// todo: consider gorilla-web
// todo: defaults
// todo: fallthroughs
