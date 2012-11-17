package main

import (
    "fmt"
    "net/http"
    "time"
)

func stream(resp http.ResponseWriter, req *http.Request) {
    respf, ok := resp.(http.Flusher)
    if !ok {
        panic("not flushable")
    }
    fmt.Println("stream")
    resp.WriteHeader(200)
    for i := 0; i < 10; i++ {
        n, err := resp.Write([]byte("tick\n"))
        respf.Flush()
        fmt.Println("tick", n, err)
        time.Sleep(time.Second * 1)
    }
}

func main() {
    http.HandleFunc("/", stream)
    fmt.Println("serve")
    http.ListenAndServe(":5000", nil)
}
