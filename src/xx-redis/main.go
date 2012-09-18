package main

import (
	"fmt"
	"github.com/fzzbt/radix/redis"
)

func main() {
	conf := redis.DefaultConfig()
	client := redis.NewClient(conf)
	fmt.Println(conf)
	fmt.Println(client)

	setRep := client.Set("foo", "bar")
	if setRep.Err != nil { panic(setRep.Err) }
	fmt.Println(setRep)

	getRep := client.Get("foo")
	if getRep.Err != nil { panic(getRep.Err) }
	getStr, _ := getRep.Str()

	fmt.Println(getRep)	
	fmt.Println(getStr)
}
