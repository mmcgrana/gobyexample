// The `filepath` package provides functions to parse
// and construct *file paths* in a way that is portable
// between operating systems; `dir/file` on Linux vs.
// `dir\file` on Windows, for example.
package main

import (
	"fmt"
	"path/filepath"
	"strings"
)

func main() {

	// `Join` should be used to construct paths in a
	// portable way. It takes any number of arguments
	// and constructs a hierarchical path from them.
	p := filepath.Join("dir1", "dir2", "filename")
	fmt.Println("p:", p)

	// You should always use `Join` instead of
	// concatenating `/`s or `\`s manually. In addition
	// to providing portability, `Join` will also
	// normalize paths by removing superfluous separators
	// and directory changes.
	fmt.Println(filepath.Join("dir1//", "filename"))
	fmt.Println(filepath.Join("dir1/../dir1", "filename"))

	// `Dir` and `Base` can be used to split a path to the
	// directory and the file. Alternatively, `Split` will
	// return both in the same call.
	fmt.Println("Dir(p):", filepath.Dir(p))
	fmt.Println("Base(p):", filepath.Base(p))

	// To check whether a path is absolute, use `IsAbs`.
	fmt.Println(filepath.IsAbs("dir/file"))
	fmt.Println(filepath.IsAbs("/dir/file"))

	filename := "config.json"

	// To find a file's extension, use `Ext`.
	ext := filepath.Ext(filename)
	fmt.Println(ext)

	// To find the file's name with the extension removed,
	// use `TrimSuffix`.
	fmt.Println(strings.TrimSuffix(filename, ext))

	// `Rel` finds a relative path between a *base* and a
	// *target*.
	rel, err := filepath.Rel("a/b", "a/b/t/file")
	if err != nil {
		panic(err)
	}
	fmt.Println(rel)

	rel, err = filepath.Rel("a/b", "a/c/t/file")
	if err != nil {
		panic(err)
	}
	fmt.Println(rel)
}
