// Go contains rich function for grab web contents. _net/http_ is the major
// library.
// Ref: [golang.org](http://golang.org/pkg/net/http/#pkg-examples).
package main

import (
	"net/http"
	"net/url"
)
import "io/ioutil"
import "fmt"
import "strings"

// keep first n lines
func keepLines(s string, n int) string {
	result := strings.Join(strings.Split(s, "\n")[:n], "\n")
	return strings.Replace(result, "\r", "", -1)
}

func main() {
	// We can use GET form to get result.
	resp, err := http.Get("http://g.cn/robots.txt")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	fmt.Println("get:\n", keepLines(string(body), 3))

	// We can use POST form to get result, too.
	resp, err = http.PostForm("http://duckduckgo.com",
		url.Values{"q": {"github"}})
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	body, err = ioutil.ReadAll(resp.Body)
	fmt.Println("post:\n", keepLines(string(body), 3))
}
