# Чтобы опробовать наш фильтр строк, сначала создайте
# файл с несколькими строчными строчками.
$ echo 'hello'   > /tmp/lines
$ echo 'filter' >> /tmp/lines

# Затем используйте фильтр строк, чтобы получить строчные
# буквы.
$ cat /tmp/lines | go run line-filters.go
HELLO
FILTER
