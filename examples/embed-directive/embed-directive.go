// `//go:embed` is a compiler directive that allows programs to include arbitrary
// files/folders in the binary. Read more about go directive
// [here](https://dave.cheney.net/2018/01/08/gos-hidden-pragmas).
package main

// If no exported identifiers is directly used from `embed` package,
// you can add an underscore `_` before the package name. In this example, we simply
// import it as we need to use the `embed.FS` type from the package.
import (
	//_ "embed"
	"embed"
)

// Since one file is defined after the directive, the compiler will only include
// this single file, followed by variable `single_file_string` to access as `string` type.
//go:embed example_folder/single_file.txt
var single_file_string string

// Here is a similar example but include single file as `[]byte`.
//go:embed example_folder/single_file.txt
var single_file_byte []byte

// We can also embed multiple files or even folders with wildcard.
//go:embed example_folder/single_file.txt
//go:embed example_folder/*.hash
var folder_FS embed.FS

func main() {

	// Print out the content of `example_single_file.txt` as `string`.
	print(single_file_string)

	// Now handle `[]byte`.`
	print(string(single_file_byte))

	// Retrieve the file(s) matching `*.hash` pattern by reading from `folder_FS` first,
	// then print out.
	hash_file1 := "example_folder/multi_file1.hash"
	hash_file2 := "example_folder/multi_file2.hash"
	hash_content1, _ := folder_FS.ReadFile(hash_file1)
	hash_content2, _ := folder_FS.ReadFile(hash_file2)
	print(string(hash_content1))
	print(string(hash_content2))

}
