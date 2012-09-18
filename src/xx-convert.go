package main

import ("fmt"; "strconv")

func main() {
	b, _ := strconv.ParseBool("true")
	fmt.Println(b)

	f, _ := strconv.ParseFloat("1.234", 64)
	fmt.Println(f)

	i, _ := strconv.ParseInt("123", 0, 64)
	fmt.Println(i)

	d, _ := strconv.ParseInt("0x1b3e", 0, 64)
	fmt.Println(d)

	k, _ := strconv.Atoi("456")
	fmt.Println(k)
}
