package main

import (
	// "container/list"
	// "fmt"
	"testing"
)

func TestGenC(t *testing.T) {
	testFunc := testGenFactory(t, "", CaptureFunc)
	testFunc("func main() {}")
	testFunc("func main() int {\nreturn 0\n}")
}

func testGen(t *testing.T, input, name string, capture interface {}) {
	cgen := NewCGenerator()
	grams := parseToGrams(input)
	if _, ok := parseGrams(nil, grams, grams.Front(), capture); ok {
		if gram, ok := grams.Front().Value.(Gram); ok {
			gen := NewGen(C_ROOT)
			cgen.Gen(gen, gram)
			// fmt.Printf("Input: %s\nOutput: %s\n", input, cgen.String(gen))
		}
	} else {
		t.Errorf("Captures %s. Input: %s", name, input)
	}
}

func testGenFactory(t *testing.T, name string, capture interface {},
	) func(string) {
	return func(input string) {
		testGen(t, input, name, capture)
	}
}
