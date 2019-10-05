// Условные операторы `if` и` else` в Go
// выглядят достаточно стандартно

package main

import "fmt"

func main() {

	// Стандартное использование
	if 7%2 == 0 {
		fmt.Println("7 is even")
	} else {
		fmt.Println("7 is odd")
	}

	// Вы можете использовать блоке `if` без блока `else`.
	if 8%4 == 0 {
		fmt.Println("8 is divisible by 4")
	}

	// Присваивание переменной может происходить до условия.
	// Любые определенные значения будут доступны в
	// последующих ветках
	if num := 9; num < 0 {
		fmt.Println(num, "is negative")
	} else if num < 10 {
		fmt.Println(num, "has 1 digit")
	} else {
		fmt.Println(num, "has multiple digits")
	}
}

// Имейте ввиду, что в Go не надо использовать скобки в условии,
// но блок необходимо заключать в фигурные скобки
