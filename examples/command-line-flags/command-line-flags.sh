# Чтобы поэкспериментировать с программой флагов командной
# строки, лучше сначала скомпилировать ее, а затем
# напрямую запустить полученный бинарный файл.
$ go build command-line-flags.go

# Попробуйте встроенную программу, сначала задав ей
# значения для всех флагов.
$ ./command-line-flags -word=opt -numb=7 -fork -svar=flag
word: opt
numb: 7
fork: true
svar: flag
tail: []

# Обратите внимание, что если вы опустите флаги, они
# автоматически примут свои значения по умолчанию.
$ ./command-line-flags -word=opt
word: opt
numb: 42
fork: false
svar: bar
tail: []

# Конечные позиционные аргументы могут быть
# предоставлены после любых флагов.
$ ./command-line-flags -word=opt a1 a2 a3
word: opt
...
tail: [a1 a2 a3]

# Обратите внимание, что пакет `flag` требует, чтобы все
# флаги отображались перед позиционными аргументами
# (в противном случае флаги будут интерпретироваться
# как позиционные аргументы).
$ ./command-line-flags -word=opt a1 a2 a3 -numb=7
word: opt
numb: 42
fork: false
svar: bar
tail: [a1 a2 a3 -numb=7]

# Используйте флаги `-h` или `--help`, чтобы получить
# автоматически сгенерированный текст справки для
# программы командной строки.
$ ./command-line-flags -h
Usage of ./command-line-flags:
  -fork=false: a bool
  -numb=42: an int
  -svar="bar": a string var
  -word="foo": a string

# Если вы укажете флаг, который не был указан для пакета
# флагов, программа напечатает сообщение об ошибке
# и снова покажет текст справки.
$ ./command-line-flags -wat
flag provided but not defined: -wat
Usage of ./command-line-flags:
...
