package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
	"strings"
)

type Authenticator func(string, string) bool

func CheckAuth(r *http.Request, auth Authenticator) bool {
	s := strings.SplitN(r.Header.Get("Authorization"), " ", 2)
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

func RequireAuth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("WWW-Authenticate", `Basic realm="private"`)
	w.WriteHeader(401)
	w.Write([]byte("401 Unauthorized\n"))
}

func WithAuth(h http.HandlerFunc, a Authenticator) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
    	if CheckAuth(r, a) {
		  h(w, r)
		} else {
		  RequireAuth(w, r)
		}
	}
}

func handle(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintln(w, "Hello secret world!")
}

func main() {
	authenticator := func(_, password string) bool {
		return password == "supersecret"
	}
    http.HandleFunc("/", WithAuth(handle, authenticator))
    http.ListenAndServe(":5000", nil)
}
