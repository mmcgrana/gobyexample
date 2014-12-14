package main

import (
	"fmt"
	"os"
	"net"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "Usage: %s host:port ", os.Args[0])
		os.Exit(1)
	}
	srvAddr, err := net.ResolveTCPAddr("tcp", os.Args[1])
	checkError(err)
	conn, err := net.DialTCP("tcp", nil, srvAddr)
	checkError(err)
	for {
		var input string
		n, err := fmt.Scanln(&input)
		checkError(err)
		if n==0 {
			break
		}
		_, err = conn.Write([]byte(input))
		checkError(err)
		output := make([]byte, 1024)
		n, err = conn.Read(output)
		checkError(err)
		fmt.Println("server:", string(output))
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintln(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
