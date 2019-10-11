// Go имеет встроенную поддержку [base64
// кодирования и декодирования](http://en.wikipedia.org/wiki/Base64).

package main

// Этот синтаксис импортирует пакет `encoding/base64` с
// с алиасом `b64`, вместо названия по-умолчанию. Это
// сэкономит нам немного места.
import (
	b64 "encoding/base64"
	"fmt"
)

func main() {

	// `Строка`, которую мы будем кодировать/декодировать.
	data := "abc123!?$*&()'-=@~"

	// Go поддерживает оба стандарта и URL-совместимого
	// base64. Кодируем, используя стандартнай кодировщик.
	// Кодировщик требует `[]byte` на входе, поэтому
	// мы конвертируем нашу `строку`.
	sEnc := b64.StdEncoding.EncodeToString([]byte(data))
	fmt.Println(sEnc)

	// Декодирование может вернуть ошибку, которую можно
	// проверить, если вы не уверены в корректности
	// входных данных.
	sDec, _ := b64.StdEncoding.DecodeString(sEnc)
	fmt.Println(string(sDec))
	fmt.Println()

	// Это кодирование/декодирование использует
	// URL-совместимый base64 формат.
	uEnc := b64.URLEncoding.EncodeToString([]byte(data))
	fmt.Println(uEnc)
	uDec, _ := b64.URLEncoding.DecodeString(uEnc)
	fmt.Println(string(uDec))
}
