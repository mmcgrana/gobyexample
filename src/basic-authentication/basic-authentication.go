package main

import (
    "encoding/base64"
    "fmt"
    "net/http"
    "strings"
)

type Auth func(string, string) bool
type handler http.HandlerFunc

func testAuth(r *http.Request, auth Auth) bool {
    header := r.Header.Get("Authorization")
    s := strings.SplitN(header, " ", 2)
    if len(s) != 2 || s[0] != "Basic" {
        return false
    }
    b, err := base64.StdEncoding.DecodeString(s[1])
    if err != nil {
        return false
    }
    pair := strings.SplitN(string(b), ":", 2)
    if len(pair) != 2 {
        return false
    }
    return auth(pair[0], pair[1])
}

func requireAuth(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("WWW-Authenticate",
        "Basic realm=\"private\"")
    w.WriteHeader(401)
    w.Write([]byte("401 Unauthorized\n"))
}

func wrapAuth(h handler, a Auth) handler {
    return func(w http.ResponseWriter, r *http.Request) {
        if testAuth(r, a) {
            h(w, r)
        } else {
            requireAuth(w, r)
        }
    }
}

func hello(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintln(w, "Hello secret world!")
}

func main() {
    checkPassword := func(_, password string) bool {
        return password == "supersecret"
    }
    handler1 := http.HanderFunc(hello)
    handler2 := wrapAuth(handler1, checkPassword)
    http.ListenAndServe(":5000", handler2)
}
