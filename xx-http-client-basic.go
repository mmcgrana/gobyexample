package main

import ("net/http"; "io/ioutil"; "fmt")

func main() {
	req, _ := http.NewRequest("GET", "http://127.0.0.1:5000/", nil)
	req.SetBasicAuth("u", "supersecret")
	res, resErr := http.DefaultClient.Do(req)
	if resErr != nil { panic(resErr) }
	defer res.Body.Close()
	body, bodyErr := ioutil.ReadAll(res.Body)
	if bodyErr != nil { panic(bodyErr) }
	fmt.Print(string(body))
}
