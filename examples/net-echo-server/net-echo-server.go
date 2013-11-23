package main

import (
	"fmt"
	"net"
	"os"
)

func main() {
	srvAddr, err := net.ResolveTCPAddr("tcp", ":9090")
	checkError(err)
	listenfd, err := net.ListenTCP("tcp", srvAddr)
	checkError(err)
	for {
		conn, err := listenfd.Accept()
		if err != nil {
			continue
		}
		go handleClient(conn)
	}
}

func handleClient(conn net.Conn) {
	defer conn.Close()
	for {
		bytes := make([]byte, 1024)
		n, err := conn.Read(bytes)
		checkError(err)
		if n==0 {
			break
		}
		fmt.Println(string(bytes))
		n, err = conn.Write(bytes)
		checkError(err)
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Println(os.Stderr, "Fatal error: ", err.Error())
		os.Exit(1)
	}
}
