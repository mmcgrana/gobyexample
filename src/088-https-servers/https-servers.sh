$ cd /tmp
$ rm -f server.*
$ openssl genrsa -des3 -out server.orig.key 2048
$ openssl rsa -in server.orig.key -out server.key
$ openssl req -new -key server.key -out server.csr
$ openssl x509 -req -days 365 -in server.csr \
    -signkey server.key -out server.crt

$ go run https-servers.go

$ curl https://127.0.0.1:5000/
$ curl --insecure https://127.0.0.1:5000/
