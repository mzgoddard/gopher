package main

import (
	"bytes"
	"container/list"
	"io/ioutil"
	"path"
	"strings"
)

type GenCode int

type Gen struct {
	code GenCode
	name string
	value string
	gram Gram
	parent *Gen
	children *list.List
}

type GenIter chan *Gen

type GenTable map[GramCode]func(*Gen, Gram)

type Generator struct {
	codeFuncTable map[GramCode]func(*Gen, Gram)
}

type CGenerator struct {
	Generator
}

const (
	C_NONE = iota
	C_EOF
	C_TOKEN
	C_INDENT
	C_INDENT_BLOCK
	C_TYPE
	C_LITERAL_INT
	C_LITERAL
	C_EXPR
	C_RETURN
	C_STATEMENT
	C_FUNC_ARG
	C_FUNC_RETURN
	C_FUNC
	C_HEADER
	C_SOURCE
	C_ROOT
)

func NewGen(code GenCode) *Gen {
	gen := new(Gen)
	*gen = Gen {
		code: code,
		name: "",
		value: "",
		parent: nil,
		children: list.New(),
	}
	return gen
}

func (self *Gen) NewGram(code GenCode, name string, gram Gram) *Gen {
	gen := NewGen(code)
	gen.parent = self
	gen.name = name
	gen.gram = gram
	return gen
}

func (self *Gen) NewToken(code GenCode, name, value string) *Gen {
	gen := NewGen(code)
	gen.parent = self
	gen.name = name
	gen.value = value
	return gen
}

func (self *Gen) PushToken(value string) *Gen {
	gen := self.NewToken(C_TOKEN, "", value)
	self.children.PushBack(gen)
	return self
}

func (self *Gen) AddSpace() *Gen {
	return self.PushToken(" ")
}

func (self *Gen) AddLine() *Gen {
	return self.PushToken("\n")
}

func (self *Gen) IndentBlock() *Gen {
	gen := self.NewGram(C_INDENT_BLOCK, "", nil)
	self.children.PushBack(gen)
	return gen
}

func (self *Gen) AddIndent() *Gen {
	gen := self.NewGram(C_INDENT, "", nil)
	self.children.PushBack(gen)
	return self
}

func (self *Gen) Root(code GenCode) *Gen {
	return self.IterParents().Last()
}

func (self *Gen) FindParent(code GenCode) *Gen {
	return self.IterParents().Filter(code).First()
}

func (self *Gen) FindNamedParent(code GenCode, name string) *Gen {
	return self.IterParents().FilterNamed(code, name).First()
}

func (self *Gen) Find(code GenCode) *Gen { return nil }
func (self *Gen) FindNamed(code GenCode, name string) *Gen { return nil }

func (self *Gen) IterSiblings() GenIter { return nil }

func (self *Gen) Iter() GenIter {
	c := make(GenIter)
	go func() {
		for e := self.children.Front(); e != nil; e = e.Next() {
			if gen, ok := e.Value.(*Gen); ok {
				c <- gen
			}
		}
		close(c)
	}()
	return c
}

func (self *Gen) IterParents() GenIter {
	c := make(GenIter)
	go func() {
		parent := self.parent
		for parent != nil {
			c <- parent
			parent = parent.parent
		}
		close(c)
	}()
	return c
}

func (self *Gen) Walk() GenIter { return nil }

func (self GenIter) FilterFunc(test func(*Gen) bool) GenIter {
	c := make(GenIter)
	go func() {
		for gen := range self {
			if test(gen) {
				c <- gen
			}
		}
		close(c)
	}()
	return c
}

func (self GenIter) Filter(code GenCode) GenIter {
	return self.FilterFunc(func(gen *Gen) bool {
		return gen.code == code
	})
}

func (self GenIter) FilterNamed(code GenCode, name string) GenIter {
	return self.FilterFunc(func(gen *Gen) bool {
		return gen.code == code && gen.name == name
	})
}

func (self GenIter) First() *Gen {
	if first, ok := <-self; ok {
		return first
	}
	return nil
}

func (self GenIter) Last() *Gen {
	var last *Gen = nil
	for last = range self {}
	return last
}

func (self GenIter) Count() int {
	i := 0
	for _ = range self {
		i++
	}
	return i
}

func NewCGenerator() *CGenerator {
	codeFuncTable := make(map[GramCode]func(*Gen, Gram))
	generator := new(CGenerator)
	*generator = CGenerator {
		Generator: Generator {
			codeFuncTable: codeFuncTable,
		},
	}

	codeFuncTable[GRAM_FUNC] = generator.genFunc

	return generator
}

func (self *CGenerator) String(gen *Gen) string {
	s := ""
	for subgen := range gen.Iter() {
		if subgen.code == C_TOKEN {
			s += subgen.value
		} else if subgen.code == C_INDENT {
			indents := gen.IterParents().Filter(C_INDENT_BLOCK).Count()
			for indents > 0 {
				indents--
				s += "  "
			}
		} else {
			s += self.String(subgen)
		}
	}
	return s
}

func (self *CGenerator) Output(gen *Gen, dir string) {
	for subgen := range gen.Iter() {
		if subgen.code == C_HEADER || subgen.code == C_SOURCE {
			filePath := path.Join(dir, subgen.name)
			ioutil.WriteFile(
				filePath,
				bytes.NewBufferString(self.String(subgen)).Bytes(),
				0666,
			)
		}
	}
}

func (self *CGenerator) Gen(gen *Gen, gram Gram) {
	self.gen(gen, gram, GenTable {
		GRAM_FUNC: self.genFunc,
		GRAM_MODULE: self.genModule,
	})
}

func (self *CGenerator) gen(parent *Gen, gram interface {}, table GenTable) {
	if realGram, ok := gram.(Gram); ok {
		if fn, ok := table[realGram.Code()]; ok {
			fn(parent, realGram)
		}
	} else if gramE, ok := gram.(*list.Element); ok {
		self.gen(parent, gramE.Value, table)
	}
}

func (self *CGenerator) genAll(parent *Gen, gram Gram, table GenTable) {
	for subgram := range NewGramIter(gram) {
		if fn, ok := table[subgram.Code()]; ok {
			fn(parent, subgram)
		}
	}
}

func (self *CGenerator) genFirst(parent *Gen, gram Gram, table GenTable) {
	for subgram := range NewGramIter(gram) {
		if fn, ok := table[subgram.Code()]; ok {
			fn(parent, subgram)
			break
		}
	}
}

func (self *CGenerator) genType(gen *Gen, gram Gram) {
	if gram == nil {
		gen.PushToken("void")
		return
	}

	for subgram := range NewGramIter(gram) {
		if subgram.Code() == GRAM_TOKEN_NAME {
			gen.PushToken(GramTokenString(subgram))
		} else if subgram.Code() == GRAM_TYPE {
			self.genType(gen, subgram)
		}
	}
}

func (self *CGenerator) genLiteral(parent *Gen, gram Gram) {
	self.genFirst(parent, gram, GenTable {
		GRAM_TOKEN_INT: func(parent *Gen, gram Gram) {
			gen := parent.NewGram(C_LITERAL_INT, "", gram)
			parent.children.PushBack(gen)
			gen.PushToken(GramTokenString(gram))
		},
	})
}

func (self *CGenerator) genExpr(parent *Gen, gram Gram) {
	self.genFirst(parent, gram, GenTable {
		GRAM_LITERAL: self.genLiteral,
	})
}

func (self *CGenerator) genReturn(parent *Gen, gram Gram) {
	gen := parent.NewGram(C_RETURN, "", nil)
	parent.children.PushBack(gen)
	gen.PushToken("return")

	exprGram := GramFind(gram, GRAM_EXPR)
	if exprGram != nil {
		gen.AddSpace()
		self.genExpr(gen, exprGram)
	}

	gen.PushToken(";")
}

func (self *CGenerator) genStatement(parent *Gen, gram Gram) {
	gen := parent.NewGram(C_STATEMENT, "", nil)
	parent.children.PushBack(gen)

	gen.AddIndent()
	self.gen(gen, gram.Children().Front(), GenTable {
		GRAM_RETURN: self.genReturn,
	})
	gen.AddLine()
}

func (self *CGenerator) genFuncArg(parent *Gen, gram Gram) {
	
}

func (self *CGenerator) genFunc(parent *Gen, gram Gram) {
	nameGram := GramFind(gram, GRAM_TOKEN_NAME)
	// receiverGram := GramFind(gram, GRAM_FUNC_RECEIVER)
	name := GramTokenString(nameGram)
	funcGen := parent.NewGram(C_FUNC, name, gram)
	parent.children.PushBack(funcGen)

	funcReturn := GramFind(gram, GRAM_FUNC_RETURN)
	self.genType(funcGen, funcReturn)
	funcGen.AddSpace()

	funcGen.PushToken(name).PushToken("(")

	for arg := range NewGramIter(gram).Filter(GRAM_FUNC_ARG) {
		self.genFuncArg(funcGen, arg)
	}

	funcGen.PushToken(")").AddSpace().PushToken("{").AddLine()
	blockGen := funcGen.IndentBlock()

	for statement := range NewGramIter(gram).Filter(GRAM_STATEMENT) {
		self.genStatement(blockGen, statement)
	}

	funcGen.PushToken("}").AddLine().AddLine()
}

func (self *CGenerator) genModule(parent *Gen, gram Gram) {
	if gramBase, ok := gram.(*GramBase); ok {
		name := gramBase.name
		headerGen := parent.NewGram(C_HEADER, fileNameAsHeader(name), gram)
		parent.children.PushBack(headerGen)
		sourceGen := parent.NewGram(C_SOURCE, fileNameAsSource(name), gram)
		parent.children.PushBack(sourceGen)

		self.genAll(sourceGen, gram, GenTable {
			GRAM_FUNC: self.genFunc,
		})
	}
}

func fileNamePrefix(fileName string) string {
	index := strings.LastIndex(fileName, ".")
	if index != -1 {
		return fileName[:index]
	}
	return fileName
}

func fileNameAsHeader(fileName string) string {
	return fileNamePrefix(fileName) + ".h"
}

func fileNameAsSource(fileName string) string {
	return fileNamePrefix(fileName) + ".c"
}
