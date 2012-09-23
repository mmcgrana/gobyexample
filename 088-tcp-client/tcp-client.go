// ## TCP Client

package main

import "net"
import "fmt"

func main() {
    c, err := net.Dial("tcp", "127.0.0.1:5000")
    if err != nil {
        panic(err)
    }

    msg := "hello world"
    fmt.Println("Sending: ", msg)
    _, err = c.Write([]byte(msg))
    if err != nil {
        panic(err)
    }

    buf := make([]byte, 1024)
    _, err = c.Read(buf)
    if err != nil {
        panic(err)
    } else {
        fmt.Println("Received:", string(buf))
        c.Close()
    }
}
