// File name parsing could did by _path_ package.
// Ref: [golang.org](http://golang.org/pkg/path/#pkg-examples)

package main

import (
	"fmt"
	"path"
)

func main() {
	fmt.Println(path.Dir("/a/b/c/d.e"))

	// Note: Ext() get the extra dot (eg. `.e` instead of `e`)
	fmt.Println(path.Ext("/a/b/c/d.e"))
	fmt.Println(path.Base("/a/b/c/d.e"))
	fmt.Println(path.Split("/a/b/../c/d.e"))
	fmt.Println(path.Clean("/a/b/../c/d.e"))
}
