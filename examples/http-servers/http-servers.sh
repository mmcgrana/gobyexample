# Запускаем сервер в фоне.
$ go run http-servers.go &

# Делаем запрос по адресу `/hello`.
$ curl localhost:8090/hello
hello
