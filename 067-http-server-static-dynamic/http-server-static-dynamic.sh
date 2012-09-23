$ cd src
$ go run xx-http-server-static-dynamic.go

$ curl http://127.0.0.1:5000/hello
$ curl http://127.0.0.1:5000/static
$ curl http://127.0.0.1:5000/static/01-hello.go
