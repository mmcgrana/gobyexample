// Go предлагает встроенную поддержку кодирования и
// декодирования JSON, в том числе встроенных и
// пользовательских типов данных.

package main

import (
	"encoding/json"
	"fmt"
	"os"
)

// Мы будем использовать эти две структуры, для демонстрации
// кодирования и декодирования.
type response1 struct {
	Page   int
	Fruits []string
}

// Только экспортируемые поля могут быть кодированы и
// декодированы в JSON. Поля должны начинаться с
// заглавной буквы.
type response2 struct {
	Page   int      `json:"page"`
	Fruits []string `json:"fruits"`
}

func main() {

	// Для начала мы рассмотрим кодирование данных в
	// JSON строку. Вот несколько примеров для простых
	// типов данных.
	bolB, _ := json.Marshal(true)
	fmt.Println(string(bolB))

	intB, _ := json.Marshal(1)
	fmt.Println(string(intB))

	fltB, _ := json.Marshal(2.34)
	fmt.Println(string(fltB))

	strB, _ := json.Marshal("gopher")
	fmt.Println(string(strB))

	// А вот примеры для срезов и карт, которые кодируются
	// в JSON массивы и объекты, как мы и ожидаем.
	slcD := []string{"apple", "peach", "pear"}
	slcB, _ := json.Marshal(slcD)
	fmt.Println(string(slcB))

	mapD := map[string]int{"apple": 5, "lettuce": 7}
	mapB, _ := json.Marshal(mapD)
	fmt.Println(string(mapB))

	// Пакет JSON может автоматически кодировать ваши
	// пользовательские типы данных. Он будет включать
	// только экспортируемые поля в закодированный
	// вывод и по умолчанию будет использовать эти
	// имена в качестве ключей JSON.
	res1D := &response1{
		Page:   1,
		Fruits: []string{"apple", "peach", "pear"}}
	res1B, _ := json.Marshal(res1D)
	fmt.Println(string(res1B))

	// Вы можете использовать теги в объявлениях
	// структурных полей для настройки кодированных имен
	// ключей JSON. Проверьте определение `response2`
	// выше, чтобы увидеть пример таких тегов.
	res2D := &response2{
		Page:   1,
		Fruits: []string{"apple", "peach", "pear"}}
	res2B, _ := json.Marshal(res2D)
	fmt.Println(string(res2B))

	// Теперь давайте рассмотрим декодирование данных
	// JSON в значения Go. Вот пример для общей
	// структуры данных.
	byt := []byte(`{"num":6.13,"strs":["a","b"]}`)

	// Нам нужно предоставить переменную, в которую пакет
	// JSON может поместить декодированные данные.
	// `map[string]interface{} будет содержать карту
	// строк для произвольных типов данных.
	var dat map[string]interface{}

	// Вот фактическое декодирование и проверка на наличие
	// ошибок.
	if err := json.Unmarshal(byt, &dat); err != nil {
		panic(err)
	}
	fmt.Println(dat)

	// Чтобы использовать значения в декодированной карте,
	// нам нужно преобразовать их в соответствующий тип.
	// Например, здесь мы конвертируем значение из `num`
	// в ожидаемый тип `float64`.
	num := dat["num"].(float64)
	fmt.Println(num)

	// Доступ к вложенным данным требует ряда преобразований.
	strs := dat["strs"].([]interface{})
	str1 := strs[0].(string)
	fmt.Println(str1)

	// Мы также можем декодировать JSON в пользовательские
	// типы данных. Это дает преимущество добавления
	// дополнительной безопасности типов в наши программы
	// и устранения необходимости в определении типрв
	// при доступе к декодированным данным.
	str := `{"page": 1, "fruits": ["apple", "peach"]}`
	res := response2{}
	json.Unmarshal([]byte(str), &res)
	fmt.Println(res)
	fmt.Println(res.Fruits[0])

	// В приведенных выше примерах мы всегда использовали
	// байты и строки в качестве промежуточных звеньев между
	// данными и представлением JSON на стандартном выходе.
	// Мы также можем транслировать JSON-кодировки напрямую
	// в `os.Writer`, такие как `os.Stdout` или даже HTTP-тела
	// ответа.
	enc := json.NewEncoder(os.Stdout)
	d := map[string]int{"apple": 5, "lettuce": 7}
	enc.Encode(d)
}
