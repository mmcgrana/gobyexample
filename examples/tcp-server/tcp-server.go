// Basic TCP servers listen for connections and
// accept them in a loop using the `net` package.
package main

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"strings"
)

func main() {
	// `net.Listen` starts the server on the given network
	// (tcp) and address (port 8090 on all interfaces).
	listener, err := net.Listen("tcp", ":8090")
	if err != nil {
		log.Fatal("Error listening:", err)
	}

	// Close the listener to free the port
	// when the application exits.
	defer listener.Close()

	// Loop indefinitely to accept new client connections.
	for {
		// `listener.Accept()` blocks the loop until a client
		// attempts to connect, returning a net.Conn.
		conn, err := listener.Accept()
		if err != nil {
			log.Println("Error accepting conn:", err)
			continue
		}

		// We use a goroutine here to handle the connection
		// so that the main loop can immediately return
		// and accept the next connection.
		go handleConnection(conn)
	}
}

// `handleConnection` implements the core server logic.
// It is called for each client connection in its own
// goroutine, allowing the server to handle multiple
// clients concurrently.
func handleConnection(conn net.Conn) {
	// Closing the connection releases resources when
	// we are finished interacting with the client.
	defer conn.Close()

	// Use `bufio.NewReader` to easily read data sent
	// from the client until a newline character ('\n')
	// is encountered.
	reader := bufio.NewReader(conn)
	message, err := reader.ReadString('\n')
	if err != nil {
		// Log errors like EOF if the client disconnects.
		log.Printf("Read error: %v", err)
		return
	}

	// Create and send a response back to the client,
	// demonstrating two-way communication.
	ackMsg := strings.ToUpper(strings.TrimSpace(message))
	response := fmt.Sprintf("ACK: %s\n", ackMsg)
	_, err = conn.Write([]byte(response))
	if err != nil {
		log.Printf("Server write error: %v", err)
	}
}
