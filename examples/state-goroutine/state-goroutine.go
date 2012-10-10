package main

import "fmt"
import "time"
import "math/rand"
import "sync/atomic"

// 1 statefull go routine
// writers issue writes
// readers issue reads
// e.g. routing table

type readOp struct {
    key  int
    resp chan int
}

type writeOp struct {
    key  int
    val  int
    resp chan bool
}

func randKey() int {
    return rand.Intn(10)
}

func randVal() int {
    return rand.Intn(100)
}

func manageState(rs chan *readOp, ws chan *writeOp) {
    data := make(map[int]int)
    for {
        select {
        case read := <-rs:
            read.resp <- data[read.key]
        case write := <-ws:
            data[write.key] = write.val
            write.resp <- true
        }
    }
}

// Keep track of how many ops we do.
var opCount int64 = 0

// Generate random reads.
func generateReads(reads chan *readOp) {
    for {
        key := randKey()
        read := &readOp{key: key, resp: make(chan int)}
        reads <- read
        <-read.resp
        atomic.AddInt64(&opCount, 1)
    }
}

// Generate random writes.
func generateWrites(writes chan *writeOp) {
    for {
        key := randKey()
        val := randVal()
        write := &writeOp{
            key:  key,
            val:  val,
            resp: make(chan bool)}
        writes <- write
        <-write.resp
        atomic.AddInt64(&opCount, 1)
    }
}

func main() {
    reads := make(chan *readOp)
    writes := make(chan *writeOp)

    go manageState(reads, writes)

    for r := 0; r < 100; r++ {
        go generateReads(reads)
    }
    for w := 0; w < 10; w++ {
        go generateWrites(writes)
    }

    atomic.StoreInt64(&opCount, 0)
    time.Sleep(time.Second)
    finalOpCount := atomic.LoadInt64(&opCount)
    fmt.Println(finalOpCount)
}
