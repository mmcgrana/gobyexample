# Run the TCP server in the background.
$ go run tcp-server.go &

# Send data and capture the response using netcat.
$ echo "Hello from netcat" | nc localhost 8090
ACK: HELLO FROM NETCAT

