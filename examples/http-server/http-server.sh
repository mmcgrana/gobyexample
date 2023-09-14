# Run the server in the background.
$ go run http-servers.go &

# Access the `/hello` route.
$ curl localhost:8090/hello
hello


# Access the `/headers` route.
$ curl localhost:8090/headers
Accept: */*
User-Agent: curl/8.1.2
