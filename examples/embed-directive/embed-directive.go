// Starting in Go version 1.16+, Go supports use of `//go:embed`
// which is a directive to embed files and folders into
// the application binary by the compiler.

// Since Go does not have preprocessor or macros like C type of languages,
// `pragmas/directives` are implemented by the compiler as comments `//`.
// Using `//go:embed` tells the compiler to include arbitrary files/folders in binary.
package main

// If you are not using any exported identifiers from `embed` package directly,
// you can add an underscore `_` before the package name, ask Go to import `embed`
// without directly using it, like this: `_ "embed"`. In this example, we remove the
// underscore as we need to use the `embed.FS` type from the package.
import (
	"embed"
)

// Since one file is defined after the directive, the compiler will only include
// this single file, followed by variable `single_file_string` to access as `string` type.
//go:embed embed-directive.sh
var single_file_string string

// Here is a similar example but include single file as `[]byte`.
// Note that `//go:embed` can only be used in package level,
// define it in any function will result in error.
//go:embed embed-directive.sh
var single_file_byte []byte

// We can also embed multiple files or even folders with wildcard.
// Since Go resolves these files and folders during compile time, we have to use the files in
// current working directory as examples. In practice, you can embed files with `"//go:embed /path/to/folder"`
// or with another example `"//go:embed /path/to/folder/*"`. You can add multiple `//go:embed` before defining a variable.
//go:embed *.hash
//go:embed embed-directive.sh
var folder_FS embed.FS

func main() {

	// Print out the content of `embed-directive.sh` as `string`.
	println(single_file_string)

	// Print out the content of `embed-directive.sh` as `[]byte`.
	println(string(single_file_byte))

	// Print out the content of `embed-directive.hash` by reading the file from `folder_FS` first,
	// then print out the content.
	hash_file := "embed-directive.hash"
	hash_content, _ := folder_FS.ReadFile(hash_file)
	println(string(hash_content))

}
