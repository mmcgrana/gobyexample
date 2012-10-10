package main

import (
    "fmt"
    "net"
    "net/http"
    "os"
    "os/signal"
    "sync/atomic"
    "syscall"
    "time"
)

func slow(res http.ResponseWriter, req *http.Request) {
    fmt.Println("respond at=start")
    time.Sleep(time.Second * 5)
    res.Header().Set("Content-Type", "text/plain")
    fmt.Fprintln(res, "Finally.")
    fmt.Println("respond at=finish")
}

var connCount int64 = 0

type watchedConn struct {
    net.Conn
}

func (w *watchedConn) Close() error {
    atomic.AddInt64(&connCount, -1)
    return w.Conn.Close()
}

type watchedListener struct {
    net.Listener
}

func (l *watchedListener) Accept() (net.Conn, error) {
    conn, err := l.Listener.Accept()
    if err != nil {
        return nil, err
    }
    atomic.AddInt64(&connCount, 1)
    return &watchedConn{Conn: conn}, nil
}

func main() {
    stop := make(chan bool, 1)
    sig := make(chan os.Signal, 1)

    handler := http.HandlerFunc(slow)
    server := &http.Server{Handler: handler}
    fmt.Println("listen at=start")
    listener, listenErr := net.Listen("tcp", ":5000")
    if listenErr != nil {
        panic(listenErr)
    }
    wListener := &watchedListener{Listener: listener}
    fmt.Println("listen at=finish")

    go func() {
        <-stop
        fmt.Println("close at=start")
        closeErr := wListener.Close()
        if closeErr != nil {
            panic(closeErr)
        }
        fmt.Println("close at=finish")
    }()

    go func() {
        signal.Notify(
            sig, syscall.SIGINT,
            syscall.SIGTERM)
        fmt.Println("trap at=start")
        <-sig
        stop <- true
        fmt.Println("trap at=finish")
    }()

    fmt.Println("serve at=start")
    server.Serve(wListener)
    fmt.Println("serve at=finish")
    for {
        connCountCurrent := atomic.LoadInt64(&connCount)
        if connCountCurrent > 0 {
            fmt.Println("wait at=pending remaining=",
                connCountCurrent)
            time.Sleep(time.Second)
        } else {
            fmt.Println("wait at=finish remaining=",
                connCountCurrent)
            return
        }
    }
}

// todo: pull in work from gobyexample-server
// todo: factor out to cut-and-pastable against normal app
