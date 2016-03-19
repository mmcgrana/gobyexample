# Nota che, anche se gli slice sono dei tipi diversi dagli
# array, anche essi possono essere stampati tramite
# `fmt.Println`.
$ go run slices.go
emp: [  ]
set: [a b c]
get: c
len: 3
apd: [a b c d e f]
cpy: [a b c d e f]
sl1: [c d e]
sl2: [a b c d e]
sl3: [c d e f]
dcl: [g h i]
2d:  [[0] [1 2] [2 3 4]]

# Dai un'occhiata a questo [post](http://blog.golang.org/2011/01/go-slices-usage-and-internals.html)
# scritto dal team di Go per avere ulteriori dettagli sul
# design e la implementazione degli slice in Go.

# Ora che abbiamo visto array e slice, daremo un'occhiata
# all'altra struttura di dati basilare di Go: le _map_.
