# Go by Example

Набор инструментов для генерации [Go в примерах](https://gobyexample.su),
сайта который помогает изучать язык Go на примерах.

### Обзор

Сайт **Go в Примерах** генерируется на основании кода и комментариев
в файлах в папке `examples` и рендерится на основании шаблонов в папке
`templates`. Готовый сайт находится в `public`.
Программы, реализующие процесс сборки находятся в `tools`, 
вместе с некоторыми вендорными зависимостями в `vendor`.


### Building

[![Build Status](https://travis-ci.com/mmcgrana/gobyexample.svg "Travis CI status")](https://travis-ci.com/mmcgrana/gobyexample)

Для создания сайта вам понадобятся Go и Python. Выполните:

```console
$ go get github.com/russross/blackfriday
$ tools/build
$ open public/index.html
```

Непрерывное построение в цикле:

```console
$ tools/build-loop
```

### Публикация

Загрузка сайта (AWS):

```console
$ gem install aws-sdk
$ export AWS_ACCESS_KEY_ID=...
$ export AWS_SECRET_ACCESS_KEY=...
$ tools/upload
```

### Лицензии

Это работа защищена копирайтом Mark McGranaghan и соответствует лицензии
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).

Go Gopher защищен [Renée French](http://reneefrench.blogspot.com/) и соответствует лицензии
[Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/).


### Переводы

Авторские переводы сайта Go by Example доступны в:

* [Chinese](https://gobyexample.xgwang.me/) by [xg-wang](https://github.com/xg-wang/gobyexample)
* [Czech](http://gobyexamples.sweb.cz/) by [martinkunc](https://github.com/martinkunc/gobyexample-cz)
* [French](http://le-go-par-l-exemple.keiruaprod.fr) by [keirua](https://github.com/keirua/gobyexample)
* [Italian](http://gobyexample.it) by the [Go Italian community](https://github.com/golangit/gobyexample-it)
* [Japanese](http://spinute.org/go-by-example) by [spinute](https://github.com/spinute)
* [Korean](https://mingrammer.com/gobyexample/) by [mingrammer](https://github.com/mingrammer)
* [Russian](https://gobyexample.com.ru/) by [badkaktus](https://github.com/badkaktus)
* [Spanish](http://goconejemplos.com) by the [Go Mexico community](https://github.com/dabit/gobyexample)
* [Ukrainian](http://gobyexample.com.ua/) by [butuzov](https://github.com/butuzov/gobyexample)

### Thanks

Thanks to [Jeremy Ashkenas](https://github.com/jashkenas)
for [Docco](http://jashkenas.github.com/docco/), which
inspired this project.
