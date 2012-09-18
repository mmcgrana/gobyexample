package main

import (
	"fmt"
	"github.com/fzzbt/radix/redis"
	"time"
)

func main() {
	// initialize
	conf := redis.DefaultConfig()
	client := redis.NewClient(conf)
	fmt.Println(conf)
	fmt.Println(client)

	// set & get
	setRep := client.Set("foo", "bar")
	if setRep.Err != nil { panic(setRep.Err) }
	fmt.Println(setRep)
	getRep := client.Get("foo")
	if getRep.Err != nil { panic(getRep.Err) }
	getStr, _ := getRep.Str()
	fmt.Println(getRep)	
	fmt.Println(getStr)

	// varadic calls
	client.Set("foo1", "bar1")
	client.Set("foo2", "bar2")
	client.Set("foo3", "bar3")
	mgetRep := client.Mget("foo1", "foo2", "foo3")
	fmt.Println(mgetRep)

    // multi calls
	mcallRep := client.MultiCall(func(mc *redis.MultiCall) {
		mc.Set("k1", "v1")
		mc.Get("k1")
	})
	if mcallRep.Err != nil { panic(mcallRep.Err) }
	mcallVal, _ := mcallRep.Elems[1].Str()
	fmt.Println(mcallVal)

	// transactional calls
	tranRep := client.Transaction(func(mc *redis.MultiCall) {
	  mc.Set("k2", "v2")
	  mc.Get("k2")
	})
	if tranRep.Err != nil { panic(tranRep.Err) }
	tranStr, _ := tranRep.Elems[1].Str()
	fmt.Println(tranStr)

	// pubsub
	msgHdlr := func(msg *redis.Message) {
		fmt.Println(msg)
	}
	sub, subErr := client.Subscription(msgHdlr)
	if subErr != nil { panic(subErr) }
	defer sub.Close()
	sub.Subscribe("chan1", "chan2")
	sub.Psubscribe("chan*")
	client.Publish("chan1", "foo")
	sub.Unsubscribe("chan1")
	client.Publish("chan2", "bar")
	time.Sleep(time.Second)
}

// $ redis-server
// $ cd xx-redis
// $ go get
// $ ./xx-redis
