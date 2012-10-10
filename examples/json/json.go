package main

import "encoding/json"
import "fmt"

func main() {
    // data to bytes/string
    bolB, _ := json.Marshal(true)
    fmt.Println(string(bolB))

    numB, _ := json.Marshal(1)
    fmt.Println(string(numB))

    strB, _ := json.Marshal("gopher")
    fmt.Println(string(strB))

    arrD := []string{"apple", "peach", "pear"}
    arrB, _ := json.Marshal(arrD)
    fmt.Println(string(arrB))

    hshD := map[string]int{"apple": 5, "lettuce": 7}
    hshB, _ := json.Marshal(hshD)
    fmt.Println(string(hshB))

    // string to data
    byt := []byte(`{"num":6.0,"strs":["a","b"]}`)
    var dat map[string]interface{}
    err := json.Unmarshal(byt, &dat)
    if err != nil {
        panic(err)
    }
    fmt.Println(dat)

    num := dat["num"].(float64)
    fmt.Println(num)

    strs := dat["strs"].([]interface{})
    fmt.Println(strs)
}
