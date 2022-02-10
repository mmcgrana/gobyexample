// Go offers built-in support for creating
// dynamic content or showing customized output to the user called Template.

package main

// Go has two template packages. one is "text/template" for
// regular text manipulation, and another one is "html/template"
// which has the same API as "text/template" but has additional security features.
// It should be used when generating HTML.
import (
	"log"
	"os"
	"text/template"
)

func main() {

	// New creates a template with a specific name and returns a pointer to it.
	t1 := template.New("t1")

	// Parse parses its parameter as template body.
	// We use {{.}} to access the value passed to the template when it's getting executed.
	t1, err := t1.Parse("Value is {{.}}\n")
	if err != nil {
		log.Fatal(err)
	}

	// If we want to ignore the errors we can use Must function.
	// It will panic if an error occurs when parsing the template.
	t1 = template.Must(t1.Parse("Value is {{.}}\n"))

	// Execute applies parsed template to the data we pass to it and writes the output to the io.Writer.
	t1.Execute(os.Stdout, t1.Name())
	t1.Execute(os.Stdout, "some text")
	t1.Execute(os.Stdout, true)
	t1.Execute(os.Stdout, 5)
	t1.Execute(os.Stdout, []string{
		"Go",
		"Rust",
		"C++",
		"C#",
	})
	t1.Execute(os.Stdout, struct{ name string }{
		name: "Jane Doe",
	})

	// If the data is a struct we can use the {{.FieldName}} action to access its fields.
	// The fields should be exported to be accessible when template is executing.
	t2, _ := template.
		New("t2").
		Parse("Fullname: {{.Fullname}}\n")

	t2.Execute(os.Stdout, struct {
		Fullname string
	}{
		Fullname: "Jane Doe",
	})

	// The same applies to maps; with maps there is no restriction on the case of key names.
	t2.Execute(os.Stdout, map[string]string{
		"Fullname": "Mickey Mouse",
	})

	// You can use if control structure to show data conditionally.
	// The data between if block will be shown if the field is truthy.
	// Means it is not false  boolean, empty string, nil or zero length slice, nil map/pointer.
	t3, _ := template.
		New("t3").
		Parse(`{{if .Field1}}
					If block => {{.Field1}}
				{{ else if .Field2}}
					Else if block => {{.Field2}}
				{{ else }}
					Else block
				{{ end }}`)

	s := struct {
		Field1 string
		Field2 []string
	}{}

	s.Field1 = ""
	s.Field2 = []string{}
	t3.Execute(os.Stdout, s)

	s.Field1 = "Some text"
	s.Field2 = nil
	t3.Execute(os.Stdout, s)

	// Using a range action you can loop through a slice.
	// Each time the range block is getting executed dot will be set
	// to current item of slice.
	t4, _ := template.
		New("t4").
		Parse(`Range: {{ range . }}
						{{.}}
					  {{ end }}`)
	t4.Execute(os.Stdout,
		[]string{
			"Go",
			"Rust",
			"C++",
			"C#",
		})

	// You can assign and reassign a value to a variable in templates.
	t5, _ := template.
		New("t5").
		Parse(`Variables: 
					{{ $language := "go" }}
						{{ $language }}
					{{ $language = "C" }}
						{{ $language }}`)
	t5.Execute(os.Stdout, nil)
}
