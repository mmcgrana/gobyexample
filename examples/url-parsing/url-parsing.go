// URL - это [уникальный локатор ресурса](https://adam.herokuapp.com/past/2010/3/30/urls_are_the_uniform_way_to_locate_resources/).
// Рассмотрим как парсить URL в Go.

package main

import (
	"fmt"
	"net"
	"net/url"
)

func main() {

	// Мы будем разбирать этот URL, который содержит схему,
	// аутентификационные данные, хост, порт, путь, параметры
	// и фрагмент запроса.
	s := "postgres://user:pass@host.com:5432/path?k=v#f"

	// Парсим URL и убеждаемся, что нет ошибок.
	u, err := url.Parse(s)
	if err != nil {
		panic(err)
	}

	// Получаем схему
	fmt.Println(u.Scheme)

	// `User` содержит всю аутентификационную информацию; используйте
	// `Username` и `Password` если надо получить конкретное поле.
	fmt.Println(u.User)
	fmt.Println(u.User.Username())
	p, _ := u.User.Password()
	fmt.Println(p)

	// `Host` содержит поля хост и порт, если они определены.
	// Воспользуйтесь `SplitHostPort`, чтобы разделить их.
	fmt.Println(u.Host)
	host, port, _ := net.SplitHostPort(u.Host)
	fmt.Println(host)
	fmt.Println(port)

	// Так можно получить `путь` и фрагмент после `#`.
	fmt.Println(u.Path)
	fmt.Println(u.Fragment)

	// Для получения параметров в строке вида `k=v`
	// используйте `RawQuery`. Вы так же можете разобрать
	// запрос в карту. Разобранный запрос в карту из строк
	// превращается в срез строк, так первый элемент будет
	// находиться по адресу `[0]`.
	fmt.Println(u.RawQuery)
	m, _ := url.ParseQuery(u.RawQuery)
	fmt.Println(m)
	fmt.Println(m["k"][0])
}
