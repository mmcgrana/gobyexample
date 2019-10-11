# Пробуем запустить запись в файл.
$ go run writing-files.go 
wrote 5 bytes
wrote 7 bytes
wrote 9 bytes

# Потом проверим, что контент появился в файлах.
$ cat /tmp/dat1
hello
go
$ cat /tmp/dat2
some
writes
buffered

# Далее мы рассмотрим применение некоторых идей файлового
# ввода-вывода, которые мы только что видели, к потокам
# `stdin` и `stdout`.
