$ go build command-line-subcommands.go

# Первый вызов подкоманды foo.
$ ./command-line-subcommands foo -enable -name=joe a1 a2
subcommand 'foo'
  enable: true
  name: joe
  tail: [a1 a2]

# Теперь пробуем bar.
$ ./command-line-subcommands bar -level 8 a1
subcommand 'bar'
  level: 8
  tail: [a1]

# Но bar не может принимать флаги определенные для foo.
$ ./command-line-subcommands bar -enable a1
flag provided but not defined: -enable
Usage of bar:
  -level int
    	level

# Далее мы рассмотрим переменные окружения, еще один
# распространенный способ параметризации программ.
