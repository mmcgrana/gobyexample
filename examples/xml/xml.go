// Go предлагает встроенную поддержку XML и
// XML-подобных форматов с пакетом `encoding.xml`.

package main

import (
	"encoding/xml"
	"fmt"
)

// Этот тип будет сопоставлен с XML. Как и в примерах JSON,
// теги полей содержат директивы для кодера и декодера.
// Здесь мы используем некоторые особенности пакета XML:
// `XMLName` определяет имя элемента XML, представляющего
// эту структуру; `id,attr` означает, что поле `Id` является
// _атрибутом_ XML, а не вложенным элементом.
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func (p Plant) String() string {
	return fmt.Sprintf("Plant id=%v, name=%v, origin=%v",
		p.Id, p.Name, p.Origin)
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Создаем XML, представляющий наш `plant`;
	// использование `MarshalIndent` для создания более
	// читабельного вывода.
	out, _ := xml.MarshalIndent(coffee, " ", "  ")
	fmt.Println(string(out))

	// Чтобы добавить общий заголовок XML к выводу, добавьте
	// его явно.
	fmt.Println(xml.Header + string(out))

	// Используйте `Unmarhshal` для парсинга байтов с XML в
	// структуру данных. Если XML имеет неправильный формат
	// или не может быть преобразован в Plant, будет
	// возвращена описательная ошибка.
	var p Plant
	if err := xml.Unmarshal(out, &p); err != nil {
		panic(err)
	}
	fmt.Println(p)

	tomato := &Plant{Id: 81, Name: "Tomato"}
	tomato.Origin = []string{"Mexico", "California"}

	// Поле `parent>child>plant` сообщает кодировщику о
	// необходимости вложения всех `plant` в
	// `<parent><child>...`
	type Nesting struct {
		XMLName xml.Name `xml:"nesting"`
		Plants  []*Plant `xml:"parent>child>plant"`
	}

	nesting := &Nesting{}
	nesting.Plants = []*Plant{coffee, tomato}

	out, _ = xml.MarshalIndent(nesting, " ", "  ")
	fmt.Println(string(out))
}
