// ## Basic Authentication

package main

import (
	"net/http"
	"encoding/base64"
	"strings"
	"fmt"
)

type Authenticator func(string, string) bool

func testAuth(r *http.Request, auth Authenticator) bool {
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

func requireAuth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("WWW-Authenticate", `Basic realm="private"`)
	w.WriteHeader(401)
	w.Write([]byte("401 Unauthorized\n"))
}

func wrapAuth(h http.HandlerFunc, a Authenticator) http.HandlerFunc {
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
