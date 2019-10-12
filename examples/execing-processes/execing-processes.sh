# When we run our program it is replaced by `ls`.
$ go run execing-processes.go
total 16
drwxr-xr-x  4 mark 136B Oct 3 16:29 .
drwxr-xr-x 91 mark 3.0K Oct 3 12:50 ..
-rw-r--r--  1 mark 1.3K Oct 3 16:28 execing-processes.go

# Обратите внимание, что Go не предлагает классическую
# Unix функцию `форка`. Обычно это не проблема,
# так как запуск горутин, порождение процессов и
# выполнение процессов покрывают большинство случаев
# использования `форка`.
