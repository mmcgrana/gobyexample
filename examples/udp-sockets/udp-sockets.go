// UDP (User Datagram Protocol) is a connectionless
// protocol that provides a simple interface for sending
// datagrams over the network. Unlike TCP, UDP doesn't
// guarantee delivery or ordering, but it's faster and
// has less overhead.
//
// Key differences from TCP:
// - Connectionless: No connection needs to be established
//   before sending data
// - Unreliable: Packets may be lost, duplicated, or arrive
//   out of order
// - Faster: Less overhead means lower latency
// - Stateless: Each datagram is independent
//
// UDP is commonly used for:
// - DNS queries (where speed matters more than reliability)
// - Video/audio streaming (where occasional packet loss is acceptable)
// - Online gaming (where low latency is crucial)
// - Broadcasting/multicasting
//
// In this example, we'll create both a UDP server and
// client to demonstrate how to send and receive datagrams.
package main

import (
	"fmt"
	"net"
	"time"
)

// The UDP server listens for incoming datagrams and
// responds to each one. Since UDP is connectionless,
// the server doesn't maintain connection state - it
// just reads datagrams as they arrive.
func udpServer() {
	// `net.ResolveUDPAddr` creates a UDP address structure
	// from a network type ("udp") and address string.
	// ":8081" means listen on all available network
	// interfaces on port 8081. This is similar to creating
	// a TCP address, but for UDP.
	addr, err := net.ResolveUDPAddr("udp", ":8081")
	if err != nil {
		panic(err)
	}

	// `net.ListenUDP` creates a UDP connection for listening.
	// Unlike TCP's `Listen` which returns a `Listener` that
	// requires `Accept()`, UDP immediately gives us a
	// connection we can use to read and write. This is
	// because UDP doesn't have a separate "accept" step
	// since it's connectionless - datagrams can arrive
	// from any client at any time.
	conn, err := net.ListenUDP("udp", addr)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	fmt.Println("UDP Server listening on :8081")

	// Buffer to hold incoming data. UDP datagrams have a
	// maximum size (typically 65507 bytes for IPv4), so
	// we allocate a buffer large enough to hold any
	// reasonable message. In production, you might want
	// to check the received size and handle oversized
	// datagrams appropriately.
	buffer := make([]byte, 1024)

	for {
		// `ReadFromUDP` reads a datagram and returns three
		// values: the number of bytes read, the address
		// of the sender, and any error. This is different
		// from TCP where we get the address when accepting
		// the connection. With UDP, we need the sender's
		// address for each datagram so we can respond.
		// This call blocks until a datagram arrives.
		n, clientAddr, err := conn.ReadFromUDP(buffer)
		if err != nil {
			// Handle read errors. Unlike TCP, UDP read errors
			// are less common, but they can still occur
			// (e.g., if the connection is closed).
			fmt.Println("Error reading:", err)
			continue
		}

		// Extract the actual message from the buffer.
		// We only use the first `n` bytes that were
		// actually read. The rest of the buffer contains
		// garbage from previous reads or is uninitialized.
		message := string(buffer[:n])
		fmt.Printf("Received from %s: %s\n", clientAddr, message)

		// Send a response back to the client using
		// `WriteToUDP`. We use the client's address
		// returned from `ReadFromUDP` to specify where
		// to send the response. This is necessary because
		// UDP is connectionless - we need to explicitly
		// specify the destination for each datagram.
		response := fmt.Sprintf("Server received: %s", message)
		_, err = conn.WriteToUDP([]byte(response), clientAddr)
		if err != nil {
			fmt.Println("Error writing:", err)
		}
	}
}

// The UDP client sends a datagram to the server and
// waits for a response. Unlike TCP, there's no
// connection establishment phase - we just send data.
func udpClient() {
	// Resolve the server's address. This converts the
	// address string into a `*net.UDPAddr` structure
	// that can be used for UDP operations.
	serverAddr, err := net.ResolveUDPAddr("udp", "localhost:8081")
	if err != nil {
		panic(err)
	}

	// Create a UDP connection using `net.DialUDP`. The
	// first argument is the network type ("udp"), the
	// second is the local address (nil means "use any
	// available port"), and the third is the remote
	// server address. Unlike TCP's `Dial`, this doesn't
	// actually establish a connection - it just prepares
	// a socket for sending datagrams to the specified
	// address.
	conn, err := net.DialUDP("udp", nil, serverAddr)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	// Send a message to the server. With UDP, this is
	// a "fire and forget" operation - we send the
	// datagram and hope it arrives. There's no guarantee
	// of delivery, ordering, or that the server received
	// it. The message is sent as-is without any framing
	// (unlike TCP where we used newlines).
	message := "Hello from UDP client!"
	_, err = conn.Write([]byte(message))
	if err != nil {
		panic(err)
	}

	// Set a read deadline so we don't wait forever if
	// the server doesn't respond. This is especially
	// important with UDP since packets can be lost and
	// we might never receive a response. After 1 second,
	// `Read` will return an error instead of blocking.
	conn.SetReadDeadline(time.Now().Add(1 * time.Second))

	// Read the server's response. Like the server, we
	// use a buffer to hold the incoming data. We only
	// read up to the buffer size, so if the server sends
	// a larger message, it will be truncated.
	buffer := make([]byte, 1024)
	n, err := conn.Read(buffer)
	if err != nil {
		// Check if the error is due to timeout (no response
		// received) or a real error. In a production
		// application, you'd want to handle this more
		// gracefully, perhaps retrying or logging the issue.
		panic(err)
	}

	// Extract the response message from the buffer.
	// Only the first `n` bytes contain actual data.
	response := string(buffer[:n])
	fmt.Printf("Response: %s\n", response)
}

func main() {
	// Start the server in a goroutine so it runs
	// concurrently with the client code below.
	go udpServer()

	// Give the server a moment to start listening.
	// In a real application, you might use a more
	// robust synchronization mechanism, but for this
	// simple example, a short sleep is sufficient.
	time.Sleep(100 * time.Millisecond)

	// Run the client. In a real application, the
	// client would typically be a separate program
	// running on a different machine.
	udpClient()

	// Give time for the server to process the datagram
	// and send the response before the program exits.
	// Without this, the program might exit before the
	// server finishes handling the request.
	time.Sleep(100 * time.Millisecond)
}

