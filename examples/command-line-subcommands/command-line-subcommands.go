// Некоторые инструменты командной строки, такие как `go`
// или `git`, имеют много *подкоманд*, каждая со своим
// собственным набором флагов. Например, `go build` и
// `go get` - это две разные подкоманды инструмента `go`.
// Пакет `flag` позволяет нам легко определять простые
// подкоманды, которые имеют свои собственные флаги.

package main

import (
	"flag"
	"fmt"
	"os"
)

func main() {

	// Мы объявляем подкоманду, используя функцию `NewFlagSet`,
	// и приступаем к определению новых флагов, специфичных
	// для этой подкоманды.
	fooCmd := flag.NewFlagSet("foo", flag.ExitOnError)
	fooEnable := fooCmd.Bool("enable", false, "enable")
	fooName := fooCmd.String("name", "", "name")

	// Для другой подкоманды мы можем определить другие
	// флаги.
	barCmd := flag.NewFlagSet("bar", flag.ExitOnError)
	barLevel := barCmd.Int("level", 0, "level")

	// Подкоманда ожидается в качестве первого аргумента
	// программы.
	if len(os.Args) < 2 {
		fmt.Println("expected 'foo' or 'bar' subcommands")
		os.Exit(1)
	}

	// Проверяем, какая подкоманда вызвана.
	switch os.Args[1] {

	// Для каждой подкоманды мы анализируем ее собственные
	// флаги и имеем доступ к аргументам.
	case "foo":
		fooCmd.Parse(os.Args[2:])
		fmt.Println("subcommand 'foo'")
		fmt.Println("  enable:", *fooEnable)
		fmt.Println("  name:", *fooName)
		fmt.Println("  tail:", fooCmd.Args())
	case "bar":
		barCmd.Parse(os.Args[2:])
		fmt.Println("subcommand 'bar'")
		fmt.Println("  level:", *barLevel)
		fmt.Println("  tail:", barCmd.Args())
	default:
		fmt.Println("expected 'foo' or 'bar' subcommands")
		os.Exit(1)
	}
}
