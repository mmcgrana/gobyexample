// Стандартная библиотека Go поставляется с отличной
// поддержкой клиентов и серверов HTTP в пакете `net/http`.
// В этом примере мы будем использовать его для
// простых HTTP-запросов.
package main

import (
	"bufio"
	"fmt"
	"net/http"
)

func main() {

	// Отправьте HTTP-запрос GET на сервер. `http.Get` - это
	// удобный способ создания объекта `http.Client` и вызова
	// его метода `Get`; он использует объект `http.DefaultClient`,
	// который имеет полезные настройки по умолчанию.
	resp, err := http.Get("http://gobyexample.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	// Выведем статус http-ответа.
	fmt.Println("Response status:", resp.Status)

	// Выведем первые 5 строк тела ответа.
	scanner := bufio.NewScanner(resp.Body)
	for i := 0; scanner.Scan() && i < 5; i++ {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}
}
