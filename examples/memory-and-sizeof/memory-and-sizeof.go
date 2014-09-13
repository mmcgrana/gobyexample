// Let's try to figure out memory usage of Go.
package main

import (
	"fmt"
	"runtime"
	"strconv"
	"unsafe"
)

// Unlike C, there is no _#pragma pack_ in Go, the real memory allocation
// depends on its implementation.
type T struct {
	B  uint8 // is a byte
	I  int   // it is int32 on my x86 32 bit PC
	P  *int  // it is int32 on my x86 32 bit PC
	S  string
	SS []string
}

var p = fmt.Println

// In this case, the `t := T{}` can not measured by this method.
func memUsage(m1, m2 *runtime.MemStats) {
	p("Alloc:", m2.Alloc-m1.Alloc,
		"TotalAlloc:", m2.TotalAlloc-m1.TotalAlloc,
		"HeapAlloc:", m2.HeapAlloc-m1.HeapAlloc)
}
func main() {
	// Here is a tricky to get pointer size
	const PtrSize = 32 << uintptr(^uintptr(0)>>63)
	p("PtrSize=", PtrSize)
	p("IntSize=", strconv.IntSize)

	var m1, m2, m3, m4, m5, m6 runtime.MemStats
	runtime.ReadMemStats(&m1)
	t := T{}
	runtime.ReadMemStats(&m2)
	p("sizeof(uint8)", unsafe.Sizeof(t.B),
		"offset=", unsafe.Offsetof(t.B))
	p("sizeof(int)", unsafe.Sizeof(t.I),
		"offset=", unsafe.Offsetof(t.I))
	p("sizeof(*int)", unsafe.Sizeof(t.P),
		"offset=", unsafe.Offsetof(t.P))
	p("sizeof(string)", unsafe.Sizeof(t.S),
		"offset=", unsafe.Offsetof(t.S))

	// Slice is a structure of Pointer, Len and Cap.
	// Detail [here](http://blog.golang.org/go-slices-usage-and-internals)
	p("sizeof([]string)", unsafe.Sizeof(t.SS),
		"offset=", unsafe.Offsetof(t.SS))

	// We can see the this structure is 4 + 4 + 4 + 8 + 12 = 32 bytes
	// There are 3 padding bytes of first t.B expanded to 4 bytes.
	p("sizeof(T)", unsafe.Sizeof(t))

	// We will see 0 bytes, because it is on stack, so sizeof is the
	// proper method to tell how much memory allocated.
	memUsage(&m1, &m2)

	// Even string assignment is in stack.
	runtime.ReadMemStats(&m3)
	t2 := "abc"
	runtime.ReadMemStats(&m4)
	memUsage(&m3, &m4)

	// map will alloc memory in heap
	runtime.ReadMemStats(&m5)
	t3 := map[int]string{1: "x"}
	runtime.ReadMemStats(&m6)
	memUsage(&m5, &m6)

	fmt.Println(t2, t3) // prevent compiler error
}
