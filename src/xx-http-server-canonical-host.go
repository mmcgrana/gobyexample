package main

import ("net/http"; "fmt"; "strings")

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	fmt.Fprintln(res, "Hello canonical world")
}

func wrapCanonicalHost(f http.HandlerFunc, canonicalHost string) http.HandlerFunc {
	return func(res http.ResponseWriter, req *http.Request) {
		hostPort := strings.Split(req.Host, ":")
		host := hostPort[0]
		if host != canonicalHost {
			fmt.Println("redirecting from", host, "to", canonicalHost)
			hostPort[0] = canonicalHost
			url := "http://" + strings.Join(hostPort, ":") + req.URL.String()
			http.Redirect(res, req, url, 301)
		}
		f(res, req)
	}
}

func main() {
	handler := wrapCanonicalHost(hello, "localhost")
    http.HandleFunc("/", handler)
    http.ListenAndServe(":5000", nil)
}

// == running
// $ go run xx-http-server-canonical-host.go
//
// $ curl -i -L http://127.0.0.1:5000/go
// $ curl -i -L http://127.0.0.1:5000/go
