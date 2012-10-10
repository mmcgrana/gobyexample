package main

import "net/http"
import "time"
import "fmt"

func runLogging(logs chan string) {
    for log := range logs {
        fmt.Println(log)
    }
}

func wrapLogging(f http.HandlerFunc) http.HandlerFunc {
    logs := make(chan string, 10000)
    go runLogging(logs)
    return func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        f(w, r)
        method := r.Method
        path := r.URL.Path
        elapsed := float64(time.Since(start)) / 1000000.0
        logs <- fmt.Sprintf(
            "method=%s path=%s elapsed=%f",
            method, path, elapsed)
    }
}

func hello(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "text/plain")
    time.Sleep(time.Millisecond * 50)
    fmt.Fprintln(w, "Hello logged world")
}

func main() {
    handler := wrapLogging(hello)
    http.HandleFunc("/", handler)
    http.ListenAndServe(":5000", nil)
}

// todo: logging status code?
