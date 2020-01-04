# Run the server in the background.
$ go run context-in-http-servers.go &

# Simulate a client request to `/hello`, hitting
# Ctrl+C shortly after starting to signal
# cancellation.
$ curl localhost:8090/hello
server: hello handler started
^C
server: context canceled
server: hello handler ended
