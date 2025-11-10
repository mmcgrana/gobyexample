// Socket programming allows direct communication between
// applications over a network. TCP (Transmission Control
// Protocol) provides reliable, ordered, and error-checked
// delivery of data between applications.
//
// TCP is connection-oriented, meaning that before data
// can be exchanged, a connection must be established
// between client and server. Once connected, TCP ensures
// that data arrives in order and without errors, making
// it ideal for applications that require reliable data
// transfer (like web browsers, email clients, etc.).
//
// In this example, we'll create both a TCP server and
// client to demonstrate bidirectional communication.
package main

import (
	"bufio"
	"fmt"
	"net"
	"time"
)

// The TCP server listens on a port and accepts connections
// from clients. For each connection, it reads data and
// sends a response.
func tcpServer() {
	// `net.Listen` creates a TCP listener on the specified
	// address and port. The first argument "tcp" specifies
	// the network protocol, and ":8080" means listen on
	// all available network interfaces on port 8080.
	// It returns a `net.Listener` interface that can be
	// used to accept incoming connections.
	listener, err := net.Listen("tcp", ":8080")
	if err != nil {
		panic(err)
	}
	// Always close the listener when done to free up
	// the port and other resources.
	defer listener.Close()

	fmt.Println("TCP Server listening on :8080")

	// Accept connections in a loop. Each connection
	// represents a client connecting to the server.
	// This is a blocking call - the server will wait
	// here until a client connects.
	for {
		// `Accept` blocks until a new connection is made.
		// It returns a `net.Conn` interface representing
		// the connection, which provides methods for
		// reading from and writing to the client.
		conn, err := listener.Accept()
		if err != nil {
			// If there's an error accepting (e.g., the
			// listener was closed), log it and continue
			// accepting other connections.
			fmt.Println("Error accepting connection:", err)
			continue
		}

		// Handle each connection in a goroutine to allow
		// the server to accept multiple clients concurrently.
		// Without goroutines, the server would only be able
		// to handle one client at a time, blocking others
		// from connecting.
		go handleTCPConnection(conn)
	}
}

// handleTCPConnection processes a single client connection.
// It reads data from the client and sends a response back.
func handleTCPConnection(conn net.Conn) {
	// Ensure the connection is closed when this function
	// returns, freeing up resources. This is important
	// because TCP connections consume system resources.
	defer conn.Close()

	// Read data from the connection using a buffered reader.
	// `bufio.NewReader` wraps the connection with buffering,
	// which improves performance by reducing the number of
	// system calls. `ReadString('\n')` reads until it
	// encounters a newline character, which is common for
	// line-based protocols.
	reader := bufio.NewReader(conn)
	message, err := reader.ReadString('\n')
	if err != nil {
		// Handle read errors (e.g., client disconnected,
		// network issues). We return instead of panicking
		// so the server can continue handling other clients.
		fmt.Println("Error reading:", err)
		return
	}

	fmt.Printf("Received: %s", message)

	// Send a response back to the client. TCP guarantees
	// that data sent will arrive at the other end, though
	// not necessarily immediately (it may be buffered).
	// We append a newline character so the client can use
	// `ReadString('\n')` to read the complete response.
	response := fmt.Sprintf("Server received: %s", message)
	_, err = conn.Write([]byte(response + "\n"))
	if err != nil {
		fmt.Println("Error writing:", err)
	}
}

// The TCP client connects to a server and sends data.
// Unlike UDP, TCP requires establishing a connection
// before sending data.
func tcpClient() {
	// `net.Dial` establishes a TCP connection to the
	// specified address. "tcp" specifies the protocol,
	// and "localhost:8080" is the server's address.
	// This call will block until the connection is
	// established or fails. It returns a `net.Conn`
	// interface that can be used to send and receive data.
	conn, err := net.Dial("tcp", "localhost:8080")
	if err != nil {
		panic(err)
	}
	// Always close the connection when done to free
	// up resources and signal to the server that
	// we're done communicating.
	defer conn.Close()

	// Send a message to the server. TCP will handle
	// breaking the message into packets, ensuring
	// they arrive in order, and retransmitting if
	// packets are lost. We include a newline character
	// to match what the server expects to read.
	message := "Hello from TCP client!\n"
	_, err = conn.Write([]byte(message))
	if err != nil {
		panic(err)
	}

	// Read the server's response. We use the same
	// buffered reader pattern as the server for
	// consistency and efficiency.
	reader := bufio.NewReader(conn)
	response, err := reader.ReadString('\n')
	if err != nil {
		panic(err)
	}

	fmt.Printf("Response: %s", response)
}

func main() {
	// Start the server in a goroutine so it runs
	// concurrently with the client code below.
	go tcpServer()

	// Give the server a moment to start listening.
	// In a real application, you might use a more
	// robust synchronization mechanism, but for this
	// simple example, a short sleep is sufficient.
	time.Sleep(100 * time.Millisecond)

	// Run the client. In a real application, the
	// client would typically be a separate program
	// running on a different machine.
	tcpClient()

	// Give time for the server to process the connection
	// and send the response before the program exits.
	// Without this, the program might exit before the
	// server finishes handling the request.
	time.Sleep(100 * time.Millisecond)
}

