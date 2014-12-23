package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	flag.Parse()
	files := flag.Args()
	fmt.Printf("%o\n", files)

	for _, f := range files {
		fmt.Printf("%s\n", f)
		parser := NewDefaultParser()
		if fileContentBytes, error := ioutil.ReadFile(f); error == nil {
			fileContent := bytes.NewBuffer(fileContentBytes).String()
			grams := parseToGrams(fileContent)
			if _, ok := parseGrams(parser, grams, grams.Front(), CaptureModule); ok {
				fmt.Printf("Parsed %s %o.\n", f, grams.Front().Value)
				if module, ok := grams.Front().Value.(*GramBase); ok {
					parser.module = module
					module.name = f
					
					fmt.Printf("Module %s.\n", f)

					gen := NewGen(C_ROOT)
					cgen := NewCGenerator()
					cgen.Gen(gen, module)
					if wd, error := os.Getwd(); error == nil {
						fmt.Printf("Output dir %s.\n", wd)
						cgen.Output(gen, wd)
					}
				}
			} else {
				fmt.Printf("Unable to parse %s.\n", f)
			}
		}
	}
}
