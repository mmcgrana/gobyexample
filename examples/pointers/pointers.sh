# `zeroval` не изменяет значение `i` в `main`, но
# `zeroptr` изменяет, т.к. в него передается указатель
# на область памяти, в которой хранится переменная.
$ go run pointers.go
initial: 1
zeroval: 1
zeroptr: 0
pointer: 0x42131100
