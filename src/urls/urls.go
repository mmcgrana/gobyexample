package main

import "fmt"
import "net/url"
import "strings"

func main() {
    s := "postgres://user:pass@host.com:5432/path?k=v#f"
    u, err := url.Parse(s)
    if err != nil {
        panic(err)
    }
    fmt.Println(u.Scheme)
    fmt.Println(u.User)
    fmt.Println(u.User.Username())
    p, _ := u.User.Password()
    fmt.Println(p)
    fmt.Println(u.Host)
    split := strings.Split(u.Host, ":")
    fmt.Println(split[0])
    fmt.Println(split[1])
    fmt.Println(u.Path)
    fmt.Println(u.Fragment)
    fmt.Println(u.RawQuery)
    m, _ := url.ParseQuery(u.RawQuery)
    fmt.Println(m)
    fmt.Println(m["k"][0])
}
