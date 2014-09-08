// Let's try to figure out memory usage of Go.
package main

import (
	"fmt"
	"runtime"
	"unsafe"
)

type T struct {
	B uint8
	S string
	I int
}

func main() {
	var m1, m2 runtime.MemStats
	runtime.ReadMemStats(&m1)
	t := T{}
	runtime.ReadMemStats(&m2)
	fmt.Println("sizeof(uint8)", unsafe.Sizeof(t.B),
		"offset=", unsafe.Offsetof(t.B))
	fmt.Println("sizeof(string)", unsafe.Sizeof(t.S),
		"offset=", unsafe.Offsetof(t.S))
	fmt.Println("sizeof(int)", unsafe.Sizeof(t.I),
		"offset=", unsafe.Offsetof(t.I))
	fmt.Println("sizeof(T)", unsafe.Sizeof(t))
	fmt.Printf("m1:Alloc:%u TotalAlloc:%u\n", m1.Alloc, m1.TotalAlloc)
	fmt.Printf("m2:Alloc:%u TotalAlloc:%u\n", m2.Alloc, m2.TotalAlloc)
	fmt.Printf("m1:HeapAlloc:%u TotalAlloc:%u\n", m1.HeapAlloc, m1.TotalAlloc)
	fmt.Printf("m2:HeapAlloc:%u TotalAlloc:%u\n", m2.HeapAlloc, m2.TotalAlloc)
}
