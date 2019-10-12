// Иногда наши программы Go должны порождать другие, не
// Go процессы. Например, подсветка синтаксиса на этом
// сайте [реализуется](https://github.com/mmcgrana/gobyexample/blob/master/tools/generate.go)
// путем запуска [`pygmentize`](http://pygments.org/)
// процесса из программы Go. Давайте рассмотрим несколько
// примеров порождающих процессов из Go.

package main

import (
	"fmt"
	"io/ioutil"
	"os/exec"
)

func main() {

	// Мы начнем с простой команды, которая не принимает
	// аргументов или ввода и просто печатает что-то на
	// стандартный вывод. Хелпер `exec.Command` создает
	// объект для представления этого внешнего процесса.
	dateCmd := exec.Command("date")

	// `.Output` - это еще один хелпер, который обрабатывает
	// общий случай запуска команды, ожидаетее завершения
	// и сбора выходных данных. Если ошибок не было, `dateOut`
	// будет содержать байты с информацией о дате.
	dateOut, err := dateCmd.Output()
	if err != nil {
		panic(err)
	}
	fmt.Println("> date")
	fmt.Println(string(dateOut))

	// Далее мы рассмотрим несколько более сложный случай,
	// когда мы направляем данные во внешний процесс на
	// его `stdin` и собираем результаты из его `stdout`.
	grepCmd := exec.Command("grep", "hello")

	// Здесь мы явно получаем каналы ввода-вывода,
	// запускаем процесс, записываем в него некоторые
	// входные данные, читаем полученный результат и,
	// наконец, ожидаем завершения процесса.
	grepIn, _ := grepCmd.StdinPipe()
	grepOut, _ := grepCmd.StdoutPipe()
	grepCmd.Start()
	grepIn.Write([]byte("hello grep\ngoodbye grep"))
	grepIn.Close()
	grepBytes, _ := ioutil.ReadAll(grepOut)
	grepCmd.Wait()

	// Мы опускаем проверки ошибок в приведенном выше
	// примере, но вы можете использовать обычный шаблон
	// `if err != nil` для них. Мы также собираем только
	// результаты `StdoutPipe`, но вы можете собирать
	// `StderrPipe` точно таким же образом.
	fmt.Println("> grep hello")
	fmt.Println(string(grepBytes))

	// Обратите внимание, что при порождении команд нам
	// нужно предоставить явно разграниченный массив
	// команд и аргументов вместо возможности просто
	// передать одну строку командной строки.
	// Если вы хотите создать полную команду со строкой,
	// вы можете использовать опцию `-c` в `bash`:
	lsCmd := exec.Command("bash", "-c", "ls -a -l -h")
	lsOut, err := lsCmd.Output()
	if err != nil {
		panic(err)
	}
	fmt.Println("> ls -a -l -h")
	fmt.Println(string(lsOut))
}
