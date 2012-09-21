package main

import ("encoding/json"; "fmt")

func main() {
	// data to bytes/string
	bol, _ := json.Marshal(true)
	fmt.Println(string(bol))

	num, _ := json.Marshal(1)
	fmt.Println(string(num))

	str, _ := json.Marshal("gopher")
	fmt.Println(string(str))
	
	arr, _ := json.Marshal([]string{"apple", "peach", "pear"})
	fmt.Println(string(arr))

	hsh, _ := json.Marshal(map[string]int{"apple": 5, "lettuce": 7})
	fmt.Println(string(hsh))

	// string to data
	byt := []byte(`{"Name":"Wednesday","Age":6,"Parents":["Gomez","Morticia"]}`)
	var dat map[string]interface{}
	err := json.Unmarshal(byt, &dat)
	if err != nil { panic(err) }
	fmt.Println(dat)

	name := dat["Name"].(string)
	fmt.Println(name)

	parents := dat["Parents"].([]interface{})
	fmt.Println(parents)
}
