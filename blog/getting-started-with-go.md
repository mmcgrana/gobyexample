https://gist.github.com/4984b5d9fe9244776197

## Getting Started with Go and Heroku

This is an quick guide to getting started with Go. It's for experienced
programers interested in Go, and assumes you are familiar with the
terminal are using a modern Mac. It
covers everything you need to know from the [Go setup doc](http://golang.org/doc/install)
and provides additional context about environment variables and
code layouts that will help you avoid confusion.


### Download and Install Go

Go to the [downloads page](http://code.google.com/p/go/downloads/list)
and choose the link for your OS. Click on the downloaded package and
follow the quick installer.

To test your install, open a new Terminal window and try the `go`
command:

    $ go version
    go version go1.0.2


### Hello World

Here is a simple Go program. Put this in `hello.go`:

    package main

    import "fmt"

    func main() {
        fmt.Printf("hello, world\n")
    }

Now try it with `go run`:

    $ go run hello.go
    Hello, world

Great, it worked!


### Set up Go Workspace and Environment

Go expects a few particular things to be configured.
