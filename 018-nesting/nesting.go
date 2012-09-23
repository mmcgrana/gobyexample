// ## Nesting

package main

import "fmt"

func main() {
    elements := map[string]map[string]string{
        "He": map[string]string{
            "name":"Helium", 
            "state":"gas",
        },
        "Li": map[string]string{
            "name":"Lithium", 
            "state":"solid",
        },
        "Be": map[string]string{
            "name":"Beryllium", 
            "state":"solid",
        },
    }

	fmt.Println(elements)
	fmt.Println()

	li := elements["Li"]
	fmt.Println(li)
	fmt.Println()

	fmt.Println(li["name"], li["state"])
}
