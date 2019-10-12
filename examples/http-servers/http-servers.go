// Написание базового HTTP сервера очень легкореализуемо
// с пакетом `net/http`.
package main

import (
	"fmt"
	"net/http"
)

// Фундаментальная концепция серверов `net/http` - это
// *обработчики*. Обработчик - это объект, реализующий
// интерфейс `http.Handler`. Распространенным способом
// написания обработчика является использование адаптера
// `http.HandlerFunc` для функций с соответствующей
// подписью.
func hello(w http.ResponseWriter, req *http.Request) {

	// Функции, выполняющие функции обработчиков, принимают
	// в качестве аргументов `http.ResponseWriter` и
	// `http.Request`. Response writer используется для
	// наполнения HTTP-ответа. Здесь наш простой ответ
	// "hello\n".
	fmt.Fprintf(w, "hello\n")
}

func headers(w http.ResponseWriter, req *http.Request) {

	// Этот обработчик делает что-то более сложное,
	// читая все заголовки HTTP-запроса и вставляя их в
	// тело ответа.
	for name, headers := range req.Header {
		for _, h := range headers {
			fmt.Fprintf(w, "%v: %v\n", name, h)
		}
	}
}

func main() {

	// Мы регистрируем наши обработчики на сервере,
	// используя удобную функцию `http.HandleFunc`. Она
	// устанавливает *маршрут по умолчанию* в пакете
	// `net/http` и принимает функцию в качестве аргумента.
	http.HandleFunc("/hello", hello)
	http.HandleFunc("/headers", headers)

	// Наконец, мы вызываем `ListenAndServe` с портом и
	// обработчиком. nil говорит использовать только что
	// настроенный маршрутизатор по умолчанию.
	http.ListenAndServe(":8090", nil)
}
