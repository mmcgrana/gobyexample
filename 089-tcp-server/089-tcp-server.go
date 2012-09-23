package main

import ("net")

func main() {
	listener, _ := net.Listen("tcp", "0.0.0.0:5000")
	for {
		conn, _ := listener.Accept()
		go Serve(conn)
	}
}

func Serve(conn net.Conn) {
	buf := make([]byte, 1024)
	for {
		_, err := conn.Read(buf)
		if err != nil {
			conn.Close()
			return
		} else {
			conn.Write(buf)
		}
	}
}
