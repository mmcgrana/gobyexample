# Note que enquanto slices são tipos diferentes 
# de arrays, eles são exibidos de maneira similar
# pelo comando `fmt.Println`.
$ go run slices.go
vazio: [  ]
exibe slice: [a b c]
valor índice 2: c
len: 3
slice com acréscimo: [a b c d e f]
slice copiada: [a b c d e f]
slice 1: [c d e]
slice 2: [a b c d e]
slice 3: [c d e f]
slice inicializada: [g h i]
bi-dimensional:  [[0] [1 2] [2 3 4]]

# Veja esse [post](https://go.dev/blog/slices-intro)
# do time de Go para mais detalhes sobre o design e
# implementação de slices na linguagem.

# Agora que vimos arrays e slices, passaremos a estudar 
# outra estrutura de dados nativa de Go: maps.
