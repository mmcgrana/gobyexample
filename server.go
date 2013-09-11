package main

import (
	"encoding/base64"
	"fmt"
	"github.com/gorilla/mux"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync/atomic"
	"syscall"
	"time"
)

func check(err error) {
    if err != nil {
        panic(err)
    }
}

func config(k string) string {
	v := os.Getenv(k)
	if v == "" {
		panic("missing " + k)
	}
	return v
}

func runLogging(logs chan string) {
	for log := range logs {
		fmt.Println(log)
	}
}

func wrapLogging(f http.HandlerFunc, logs chan string) http.HandlerFunc {
	return func(res http.ResponseWriter, req *http.Request) {
		start := time.Now()
		f(res, req)
		method := req.Method
		path := req.URL.Path
		elapsed := float64(time.Since(start)) / 1000000.0
		logs <- fmt.Sprintf("request at=finish method=%s path=%s elapsed=%f", method, path, elapsed)
	}
}

func wrapCanonicalHost(f http.HandlerFunc, canonicalHost string, forceHttps bool) http.HandlerFunc {
	return func(res http.ResponseWriter, req *http.Request) {
		scheme := "http"
		if h, ok := req.Header["X-Forwarded-Proto"]; ok {
			if h[0] == "https" {
				scheme = "https"
			}
		}

		hostPort := strings.Split(req.Host, ":")
		host := hostPort[0]

		if (forceHttps && (scheme != "https")) || host != canonicalHost {
			if forceHttps {
				scheme = "https"
			}
			hostPort[0] = canonicalHost
			url := scheme + "://" + strings.Join(hostPort, ":") + req.URL.String()
			http.Redirect(res, req, url, 301)
			return
		}

		f(res, req)
	}
}

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

var reqCount int64 = 0

func wrapReqCount(h http.HandlerFunc, reqCountPtr *int64) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		atomic.AddInt64(reqCountPtr, 1)
		h(w, r)
		atomic.AddInt64(reqCountPtr, -1)
	}
}

func static(res http.ResponseWriter, req *http.Request) {
	http.ServeFile(res, req, "public"+req.URL.Path)
}

func notFound(res http.ResponseWriter, req *http.Request) {
	http.ServeFile(res, req, "public/404.html")
}

func checkAuth(user, pass string) bool {
	auth := os.Getenv("AUTH")
	if auth == "" {
		return true
	}
	return auth == strings.Join([]string{user, pass}, ":")
}

func routerHandlerFunc(router *mux.Router) http.HandlerFunc {
	return func(res http.ResponseWriter, req *http.Request) {
		router.ServeHTTP(res, req)
	}
}

func router() *mux.Router {
	router := mux.NewRouter()
	router.HandleFunc("/", static).Methods("GET")
	router.HandleFunc("/favicon.ico", static).Methods("GET")
	router.HandleFunc("/play.png", static).Methods("GET")
	router.HandleFunc("/site.css", static).Methods("GET")
	entries, err := ioutil.ReadDir("public")
	check(err)
	for _, f := range entries {
		if !strings.Contains(f.Name(), ".") {
			router.HandleFunc("/" + f.Name(), static).Methods("GET")
		}
	}
	router.NotFoundHandler = http.HandlerFunc(notFound)
	return router
}

func main() {
	logs := make(chan string, 10000)
	go runLogging(logs)

	handler := routerHandlerFunc(router())
	if os.Getenv("AUTH") != "" {
		handler = wrapAuth(handler, checkAuth)
	}
	handler = wrapCanonicalHost(handler, config("CANONICAL_HOST"), config("FORCE_HTTPS") == "1")
	handler = wrapLogging(handler, logs)
	handler = wrapReqCount(handler, &reqCount)

	server := &http.Server{Handler: handler}
	listener, listenErr := net.Listen("tcp", ":"+config("PORT"))
	if listenErr != nil {
		panic(listenErr)
	}

	stop := make(chan bool, 1)
	sig := make(chan os.Signal, 1)
	go func() {
		signal.Notify(sig, syscall.SIGINT, syscall.SIGTERM)
		logs <- "trap at=start"
		<-sig
		for {
			reqCountCurrent := atomic.LoadInt64(&reqCount)
			if reqCountCurrent > 0 {
				logs <- fmt.Sprintf("trap at=draining remaining=%d", reqCountCurrent)
				time.Sleep(time.Second)
			} else {
				logs <- fmt.Sprintf("trap at=finish")
				stop <- true
				return
			}
		}
	}()

	go func() {
		logs <- "serve at=start"
		server.Serve(listener)
		logs <- "serve at=finish"
	}()

	<-stop
	logs <- "close at=start"
	closeErr := listener.Close()
	if closeErr != nil {
		panic(closeErr)
	}
	logs <- "close at=finish"
}
