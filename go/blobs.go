package main

import (
	"crypto/sha1"
	"encoding/hex"
	"fmt"
)

func saveFile(checksum string, data []byte) (string, error) {
	sum := sha1.Sum(data)
	name := hex.EncodeToString(sum[:])
	if checksum != name {
		return "", fmt.Errorf("incorrect file checksum %s, expected %s", checksum, name)
	}
	fmt.Println("saving " + string(len(data)) + " bytes in file " + name)
	return name, nil
}

func saveManifest(checksum string, text []byte) (string, error) {
	sum := sha1.Sum(text)
	name := hex.EncodeToString(sum[:])
	if checksum != name {
		return "", fmt.Errorf("incorrect manifest checksum %s, expected %s", checksum, name)
	}
	fmt.Println("saving " + string(len(text)) + " bytes in manifest " + name)
	return name, nil
}
