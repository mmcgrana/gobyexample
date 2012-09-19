package main

import "net/http"

func handler(res http.ResponseWriter, req *http.Request) {
	res.Header().Set("Content-Type", "text/plain")
	res.Write([]byte("Hello from HTTPS\n"))
}

func main() {
	http.HandleFunc("/", handler)
	err := http.ListenAndServeTLS(":5000", "/tmp/server.crt", "/tmp/server.key", nil)
	if err != nil { panic(err) }
}

// $ cd /tmp
// $ rm -f server.*
// $ openssl genrsa -des3 -out server.orig.key 2048
// $ openssl rsa -in server.orig.key -out server.key
// $ openssl req -new -key server.key -out server.csr
// $ openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt

// $ go run src/xx-https-server.go

// $ curl https://127.0.0.1:5000/
// $ curl --insecure https://127.0.0.1:5000/
