package main

import ("net/http"; "fmt"; "time")

func runLogging(logs chan string) {
	for log := range logs {
		fmt.Println(log)
	}
}

func wrapLogging(f http.HandlerFunc) http.HandlerFunc {
	logs := make(chan string, 10000)
	go runLogging(logs)
	return func(res http.ResponseWriter, req *http.Request) {
		start := time.Now()
		f(res, req)
		method := req.Method
		path := req.URL.Path
		elapsed := float64(time.Since(start)) / 1000000.0
		logs <- fmt.Sprintf("method=%s path=%s elapsed=%f", method, path, elapsed)
	}
}

func hello(res http.ResponseWriter, req *http.Request) {
    res.Header().Set("Content-Type", "text/plain")
	time.Sleep(time.Millisecond * 50)
	fmt.Fprintln(res, "Hello logged world")
}

func main() {
	handler := wrapLogging(hello)
    http.HandleFunc("/", handler)
    http.ListenAndServe(":5000", nil)
}
