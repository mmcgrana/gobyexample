package main

import ("fmt"; "os")

func main() {
    file, err := os.Open("xx-file-open.go")
    if err != nil {
        panic(err)
    }
    defer file.Close()
    
    stat, err := file.Stat()
    if err != nil {
        panic(err)
    }

	fmt.Println("Program has", stat.Size(), "bytes")
}
