# Чтобы поэкспериментировать с аргументами командной
# строки, лучше сначала создать двоичный файл с
# помощью `go build`.
$ go build command-line-arguments.go
$ ./command-line-arguments a b c d
[./command-line-arguments a b c d]       
[a b c d]
c

# Далее мы рассмотрим более сложную обработку командной
# строки с флагами.
