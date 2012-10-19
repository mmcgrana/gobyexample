// State

package main

import "fmt"
import "time"
import "math/rand"
import "sync"
import "sync/atomic"
import "runtime"

// Globally-accessible state.
var data = make(map[int]int)

var dataMutex = &sync.Mutex{}

// Keep track of how many ops we do.
var opCount int64 = 0

// Generate random reads.
func generateReads() {
    total := 0
    for {
        key := rand.Intn(10)
        dataMutex.Lock()
        total += data[key]
        dataMutex.Unlock()
        atomic.AddInt64(&opCount, 1)
        runtime.Gosched()
    }
}

// Generate random writes.
func generateWrites() {
    for {
        key := rand.Intn(10)
        val := rand.Intn(100)
        dataMutex.Lock()
        data[key] = val
        dataMutex.Unlock()
        atomic.AddInt64(&opCount, 1)
        runtime.Gosched()
    }
}

func main() {
    for r := 0; r < 100; r++ {
        go generateReads()
    }
    for w := 0; w < 10; w++ {
        go generateWrites()
    }

    atomic.StoreInt64(&opCount, 0)
    time.Sleep(time.Second)
    finalOpCount := atomic.LoadInt64(&opCount)
    fmt.Println(finalOpCount)
}
