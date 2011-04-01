package main

import (
	"flag"
	"fmt"
	"http"
	"io"
	"io/ioutil"
	"os"
	"time"
)

var address *string = flag.String("a", ":8082", "address")
var evalChan chan []byte

func put(r *http.Request) {
	f, _ := os.Open("./" + r.URL.Path,
		os.O_CREAT|os.O_TRUNC|os.O_WRONLY, 0644)
	io.Copy(f, r.Body)
}

func get(c http.ResponseWriter, r *http.Request) {
	f, _ := os.Open("./" + r.URL.Path, os.O_RDONLY, 0)
	io.Copy(c, f)
}

func rootHandler(c http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		get(c, r)
	case "PUT":
		put(r)
	}
}

func logHandler(c http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		return
	}
	io.Copy(os.Stdout, r.Body)
}

func evalHandler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		w.SetHeader("Content-Type", "text/plain; charset=utf-8")
		timeoutChan := time.After(10 * 1000 * 1000 * 1000)
		select {
		case text := <-evalChan:
			_, _ = w.Write(text)
		case <-timeoutChan:
		}
	case "POST":
		text, err := ioutil.ReadAll(r.Body)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		evalChan <- text
	}
}

func init() {
	evalChan = make(chan []byte, 1)
}

func main() {
	flag.Parse()
	http.HandleFunc("/", rootHandler)
	http.HandleFunc("/log", logHandler)
	http.HandleFunc("/eval", evalHandler)
	println("Listening on", *address)
	err := http.ListenAndServe(*address, nil)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.String())
	}
}
