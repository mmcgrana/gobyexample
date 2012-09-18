package main

import ("fmt"; "os/user")

func main() {
	me, _ := user.Current()
	fmt.Println(me)
	root, _ := user.Lookup("root")
	fmt.Println(root)
}
