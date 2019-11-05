package main

import (
	"io/ioutil"
	"net/http"

	"github.com/gorilla/mux"
)

func putFile(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	vars := mux.Vars(r)
	checksum := vars["checksum"]
	data, _ := ioutil.ReadAll(r.Body) // this is not efficient
	defer r.Body.Close()
	name, _ := saveFile(checksum, data)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(name))
}

func getFile(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	vars := mux.Vars(r)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("GET FILE " + vars["checksum"] + "\n"))
}

func putManifest(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	vars := mux.Vars(r)
	checksum := vars["checksum"]
	text, _ := ioutil.ReadAll(r.Body) // this is not efficient
	defer r.Body.Close()
	name, err := saveManifest(checksum, text)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte("back checksum\n")) // assuming!
		return
	}
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(name))
}

func getManifest(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	vars := mux.Vars(r)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("GET MANIFEST " + vars["checksum"] + "\n"))
}

func getManifests(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("GET MANIFESTS\n"))
}

func notFound(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusNotFound)
	w.Write([]byte("404 Not found\n"))
}

func apiRouter(prefix string) *mux.Router {
	r := mux.NewRouter()
	api := r.PathPrefix(prefix).Subrouter()

	api.HandleFunc("/files/{checksum}", putFile).Methods(http.MethodPut)
	api.HandleFunc("/files/{checksum}", getFile).Methods(http.MethodGet)

	api.HandleFunc("/manifests/{checksum}", putManifest).Methods(http.MethodPut)
	api.HandleFunc("/manifests/{checksum}", getManifest).Methods(http.MethodGet)
	api.HandleFunc("/manifests", getManifests).Methods(http.MethodGet)

	api.HandleFunc("/", notFound)

	return r
}
