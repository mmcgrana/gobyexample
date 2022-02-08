// Go offers built-in support for creating
// dynamic content or showing customized output to the user called Template.

package main

// Go has two template packages. one is "text/template" for
// regular text manipulation, and another one is "html/template"
// mostly used in web applications.
// Here we show some of "text/template" features.
import (
	"os"
	"text/template"
)

func main() {

	// New creates a template with a specific name and returns a pointer to it.
	t := template.New("my-template")

	// Parse parses its parameter as template body.
	// We use {{.}} to access the value passed to the template when it's getting executed.
	t, _ = t.Parse("Value is {{.}}\n")

	// If we want to ignore the errors we can use Must function.
	// It will panic if an error occurs when parsing the template.
	t = template.Must(t.Parse("Value is {{.}}\n"))

	// Execute applies parsed template to the data we pass to it and writes the output to the io.Writer.
	t.Execute(os.Stdout, t.Name())
	t.Execute(os.Stdout, "some text")
	t.Execute(os.Stdout, true)
	t.Execute(os.Stdout, 5)
	t.Execute(os.Stdout, []string{
		"Go",
		"Rust",
		"C++",
		"C#",
	})
	t.Execute(os.Stdout, struct{ name string }{
		name: "Arash",
	})

	// If the data is a struct we can use the {{.FieldName}} action to access its fields.
	// The fields should be exported to be accessible when template is executing.
	t, _ = t.Parse("Firstname: {{.Firstname}}" +
		", Lastname: {{.Lastname}}\n")

	t.Execute(os.Stdout, struct {
		Firstname, Lastname string
	}{
		Firstname: "Arash",
		Lastname:  "Sameni",
	})

	// Samething applies for maps. But it's not necessary to have uppercase fields.
	t.Execute(os.Stdout, map[string]string{
		"Firstname": "Robert",
		"Lastname":  "Griesemer",
	})

	// You can use if control structure to show data conditionally.
	// The data between if block will be shown if the field is truthy.
	// Means it is not false  boolean, empty string, nil or zero length slice, nil map/pointer.
	t, _ = t.Parse(`{{if .Field1}}
						If block => {{.Field1}}
					{{ else if .Field2}}
						Else if block => {{.Field2}}
					{{ else }}
						Else block
					{{ end }}`)

	s := struct {
		Field1, Field2 interface{}
	}{}

	s.Field1 = ""
	s.Field2 = []string{}
	t.Execute(os.Stdout, s)

	s.Field1 = nil
	s.Field2 = "Some text"
	t.Execute(os.Stdout, s)

	// Using a range action you can loop through a slice.
	// Each time the range block is getting executed dot will be set
	// to current item of slice.
	// You can use $ in blocks to access outside data.
	t, _ = t.Parse(`Range: {{ range . }}
						{{.}}
					{{ end }}`)
	t.Execute(os.Stdout, []string{
		"Go",
		"Rust",
		"C++",
		"C#",
	})

	// You can assign and reassign a value to a variable in templates.
	t, _ = t.Parse(`Variables: {{ $language := "go" }}
					{{ $language }}
					{{ $language = "C" }}
					{{ $language }}`)
	t.Execute(os.Stdout, nil)
}
