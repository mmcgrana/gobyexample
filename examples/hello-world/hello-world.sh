# Per eseguire il programma, metti il codice in
# `hello-world.go` e invoca `go run` in un terminale.
$ go run hello-world.go
hello world

# A volte ci sarà la necessità di compilare i nostri
# programmi in un file binario. Possiamo fare ciò usando
# `go build`.
$ go build hello-world.go
$ ls
hello-world	hello-world.go

# Dopodiché, possiamo eseguire il binario compilato
# direttamente (senza passare per il compilatore di Go).
$ ./hello-world
hello world

# Ora che sappiamo come eseguire e compilare programmi
# basilari in Go, approfondiamo quelli che sono i
# costrutti e le strutture base del linguaggio
