$ go get code.google.com/p/gorilla/mux
$ go run xx-http-server-static-select.go

$ curl -i http://127.0.0.1:5000/
$ curl -i http://127.0.0.1:5000/favicon.ico
$ curl -i http://127.0.0.1:5000/wat
$ curl -i http://127.0.0.1:5000/wat/wat
