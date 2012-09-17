package main

import "os"

func main() {
	os.Exit(3)
}

// $ go run xx-exit.go
// exit status 3
// $ go build xx-exit.go
// $ ./xx-exit
// $ echo $?
// 3
