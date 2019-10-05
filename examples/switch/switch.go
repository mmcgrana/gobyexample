// _Switch_ помогает проверять условие в нескольких блоках

package main

import (
	"fmt"
	"time"
)

func main() {

	// Стандартное использование `switch`.
	i := 2
	fmt.Print("Write ", i, " as ")
	switch i {
	case 1:
		fmt.Println("one")
	case 2:
		fmt.Println("two")
	case 3:
		fmt.Println("three")
	}

	// Вы можете использовать запятую в качестве разделителя,
	// для перечисления нескольких значений в `case`.
	// Так же в данном примере используется блок
	// по-умолчанию `default`.
	switch time.Now().Weekday() {
	case time.Saturday, time.Sunday:
		fmt.Println("It's the weekend")
	default:
		fmt.Println("It's a weekday")
	}

	// `switch` без условия аналогичен обычному оператору
	// `if/else` по своей логике. Так же в этом примере
	// что в `case` можно использовать не только константы.
	t := time.Now()
	switch {
	case t.Hour() < 12:
		fmt.Println("It's before noon")
	default:
		fmt.Println("It's after noon")
	}

	// В этой конструкции `switch` сравниваются типы значений.
	// Вы можете использовать этот прием, для определения
	// типа значения интерфейса.
	whatAmI := func(i interface{}) {
		switch t := i.(type) {
		case bool:
			fmt.Println("I'm a bool")
		case int:
			fmt.Println("I'm an int")
		default:
			fmt.Printf("Don't know type %T\n", t)
		}
	}
	whatAmI(true)
	whatAmI(1)
	whatAmI("hey")
}
