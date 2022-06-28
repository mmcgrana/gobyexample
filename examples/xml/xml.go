// Go offers built-in support for XML and XML-like
// formats with the `encoding.xml` package.

package main

import (
	"encoding/xml"
	"fmt"
)

// Plant will be mapped to XML. Similarly to the
// JSON examples, field tags contain directives for the
// encoder and decoder. Here we use some special features
// of the XML package: the `XMLName` field name dictates
// the name of the XML element representing this struct;
// `id,attr` means that the `Id` field is an XML
// _attribute_ rather than a nested element.
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

	// Emit XML representing our plant; using
	// `MarshalIndent` to produce a more
	// human-readable output.
	out, _ := xml.MarshalIndent(coffee, " ", "  ")
	fmt.Println(string(out))

	// To add a generic XML header to the output, append
	// it explicitly.
	fmt.Println(xml.Header + string(out))

	// Use `Unmarshal` to parse a stream of bytes with XML
	// into a data structure. If the XML is malformed or
	// cannot be mapped onto Plant, a descriptive error
	// will be returned.
	var p Plant
	if err := xml.Unmarshal(out, &p); err != nil {
		panic(err)
	}
	fmt.Println(p)

	tomato := &Plant{Id: 81, Name: "Tomato"}
	tomato.Origin = []string{"Mexico", "California"}

	// The `parent>child>plant` field tag tells the encoder
	// to nest all `plant`s under `<parent><child>...`
	type Nesting struct {
		XMLName xml.Name `xml:"nesting"`
		Plants  []*Plant `xml:"parent>child>plant"`
	}

	nesting := &Nesting{}
	nesting.Plants = []*Plant{coffee, tomato}

	out, _ = xml.MarshalIndent(nesting, " ", "  ")
	fmt.Println(string(out))
}
