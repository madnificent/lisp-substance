package main

import (
	"http"
	"io/ioutil"
	"os"
)

const url = "http://192.168.2.1:9000/eval"

func main() {
	response, err := http.Post(url, "text/plain", os.Stdin)
	if err != nil {
		panic("http post fail: " + err.String())
	}
	defer response.Body.Close()
	_, err = ioutil.ReadAll(response.Body)
	if err != nil {
		panic("http post fail: " + err.String())
	}
	if response.StatusCode == 200 {
		println("groovy!")
	} else {
		println("hrm.. \"" + response.Status + "\"")
	}
}
