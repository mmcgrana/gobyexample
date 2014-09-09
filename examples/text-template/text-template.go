// text/template is a useful text generating tool.
// Related examples: http://golang.org/pkg/text/template/#pkg-examples
package main

import (
	"os"
	"text/template"
)

var card1 = map[int]string{1: "S", 2: "H", 3: "C", 4: "D"}
var card2 = map[string]string{"a": "S", "b": "H",
	"c": "C", "d": "D"}

func check(err error) {
	if err != nil {
		panic(err)
	}
}
func main() {
	// The simplest template is the "dot", just printout the variable
	t := template.Must(template.New("t1").
		Parse("Dot:{{.}}\n"))
	check(t.Execute(os.Stdout, card1))

	// The following line will fail because key should be alphanumeric.
	//`t := template.Must(template.New("greet").Parse("Hi,{{.1}}\n"))`
	//`err := t.Execute(os.Stdout, card1)`

	t = template.Must(template.New("t2").
		Parse("Hi,{{.a}}\n"))
	check(t.Execute(os.Stdout, card2))

	// Use variable to ierate map.
	t = template.Must(template.New("t3").Parse(`For
{{range $k,$v := .}}  k={{printf "%03d" $k}} v={{$v}}
{{end}}
`))
	check(t.Execute(os.Stdout, card1))

	// * logic functions:and,or,not,eq,ne,lt,le,gt,ge
	// * escape convert functions: html,js,urlquery
	// * format functions: print,printf,println,
	// * other functions: call,index,len
	t = template.Must(template.New("t2").
		Parse("{{index . 3}}\n"))
	check(t.Execute(os.Stdout, card1))

	// Use {{define}} to prevent write multiple times.
	t = template.Must(template.New("t2").Parse(`Repeat
{{define "T1"}}Apple{{end}}  {{define "T2"}}Ape{{end}}
  {{template "T2"}} ate {{template "T1"}}
`))
	check(t.Execute(os.Stdout, card1))
}
