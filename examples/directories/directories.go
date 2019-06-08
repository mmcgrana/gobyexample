// Go has several useful functions for working with
// *directories* in the file system.

package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {

	// Create a new sub-directory in the current working
	// directory.
	err := os.Mkdir("subdir", 0755)
	check(err)

	// When creating temporary directories, it's good
	// practice to `defer` their removal. `os.RemoveAll`
	// will delete a whole directory tree (similarly to
	// `rm -rf`).
	defer os.RemoveAll("subdir")

	// Helper function to create a new empty file.
	createEmptyFile := func(name string) {
		d := []byte("")
		check(ioutil.WriteFile(name, d, 0644))
	}

	createEmptyFile("subdir/file1")

	// We can create a hierarchy of directories, including
	// parents wiht `MkdirAll`. This is similar to the
	// command-line `mkdir -p`.
	err = os.MkdirAll("subdir/parent/child", 0755)
	check(err)

	createEmptyFile("subdir/parent/file2")
	createEmptyFile("subdir/parent/file3")
	createEmptyFile("subdir/parent/child/file4")

	// `ReadDir` lists directory contents, returning a
	// slice of `os.FileInfo` objects.
	c, err := ioutil.ReadDir("subdir/parent")
	check(err)

	fmt.Println("Listing subdir/parent")
	for _, entry := range c {
		fmt.Println(entry.Name(), entry.IsDir())
	}

	// `Chdir` lets us change the current working directory,
	// similarly to `cd`.
	err = os.Chdir("subdir/parent/child")
	check(err)

	// Now we'll see the contents of "subdir/parent/child"
	// when listing the *current* directory.
	c, err = ioutil.ReadDir(".")
	check(err)

	fmt.Println("Listing subdir/parent/child")
	for _, entry := range c {
		fmt.Println(entry.Name(), entry.IsDir())
	}

	// `cd` back to where we started.
	err = os.Chdir("../../..")
	check(err)
}
