package main

import (
	"log"
	"net/http"
)

func main() {
	router := apiRouter("/api")
	log.Fatal(http.ListenAndServe(":8585", router))
}
