package main

import (
	"bytes"
	"container/list"
	"fmt"
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

type GenError struct {
	message string
	gram Gram
}

type Generator struct {
	codeFuncTable map[GramCode]func(*Gen, Gram)
	errors *list.List
}

type CGenerator struct {
	Generator
}

const (
	C_NONE = iota
	C_EOF
	C_TOKEN
	C_NODE
	C_INDENT
	C_INDENT_BLOCK
	C_LINE
	C_STRUCT_MEMBER
	C_STRUCT
	C_TYPE
	C_LITERAL_INT
	C_LITERAL_REAL
	C_LITERAL_OBJECT
	C_LITERAL_MEMBER
	C_LITERAL
	C_EXPR
	C_RETURN
	C_STATEMENT
	C_FUNC_ARG
	C_FUNC_ARGS
	C_FUNC_RETURN
	C_FUNC
	C_TYPEDEF
	C_PROTOTYPE
	C_ZONE
	C_PROTOTYPE_ZONE
	C_TYPEDEF_ZONE
	C_SOURCE
	C_HEADER_START
	C_HEADER_END
	C_HEADER
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

func (self *Gen) T(value string) *Gen {
	return self.PushToken(value)
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

func (self *Gen) Line() *Gen {
	gen := self.NewGram(C_LINE, "", nil)
	self.children.PushBack(gen)
	gen.AddIndent()
	content := gen.NewGram(C_NODE, "", nil)
	gen.children.PushBack(content)
	gen.AddLine()
	return content
}

func (self *Gen) Root() *Gen {
	return self.IterParents().Last()
}

func (self *Gen) FindParent(code GenCode) *Gen {
	return self.IterParents().Filter(code).First()
}

func (self *Gen) FindNamedParent(code GenCode, name string) *Gen {
	return self.IterParents().FilterNamed(code, name).First()
}

func (self *Gen) Find(code GenCode) *Gen {
	return self.Iter().Filter(code).First()
}

func (self *Gen) FindNamed(code GenCode, name string) *Gen {
	return self.Iter().FilterNamed(code, name).First()
}

func (self *Gen) Search(code GenCode) *Gen {
	return self.Walk().Filter(code).First()
}

func (self *Gen) SearchNamed(code GenCode, name string) *Gen {
	return self.Walk().FilterNamed(code, name).First()
}

func (self *Gen) ElementAt(gen *Gen) *list.Element {
	for e := self.children.Front(); e != nil; e = e.Next() {
		if genE, ok := e.Value.(*Gen); ok {
			if genE == gen {
				return e
			}
		}
	}
	return nil
}

func (self *Gen) IterSiblings() GenIter { return nil }

func (self *Gen) Iter() GenIter {
	c := make(GenIter)
	go func() {
		defer close(c)
		for e := self.children.Front(); e != nil; e = e.Next() {
			if gen, ok := e.Value.(*Gen); ok {
				c <- gen
			}
		}
	}()
	return c
}

func (self *Gen) IterParents() GenIter {
	c := make(GenIter)
	go func() {
		defer close(c)
		parent := self.parent
		for parent != nil {
			c <- parent
			parent = parent.parent
		}
	}()
	return c
}

func (self *Gen) Walk() GenIter {
	c := make(GenIter)
	go func() {
		defer close(c)

		items := self.Iter()
		for gen := range items {
			c <- gen
			for subgen := range gen.Walk() {
				c <- subgen
			}
		}
	}()
	return c
}

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

func (self GenError) token() *Token {
	var token Gram = nil
	if gramToken, ok := self.gram.(*GramToken); ok {
		token = gramToken
	}
	if token == nil {
		token = NewGramWalk(self.gram).FilterFunc(func(gram Gram) bool {
			if _, ok := gram.(*GramToken); ok {
				return true
			}
			return false
		}).First()
	}
	if gramToken, ok := token.(*GramToken); ok {
		return gramToken.token
	}
	return nil
}

func (self GenError) Line() int {
	if token := self.token(); token != nil {
		return token.line + 1
	}
	return -1
}

func (self GenError) Column() int {
	if token := self.token(); token != nil {
		return token.column + 1
	}
	return -1
}

func (self GenError) FilePath() string {
	module := GramIterParents(self.gram).Filter(GRAM_MODULE).First()
	if moduleBase, ok := module.(*GramBase); ok {
		return moduleBase.filePath
	}
	return ""
}

func (self GenError) String() string {
	return fmt.Sprintf(
		"%s %d %d: %s\n",
		self.FilePath(), self.Line(), self.Column(), self.message,
	)
}

func (self *Generator) addError(message string, gram Gram) {
	e := new(GenError)
	*e = GenError {
		message,
		gram,
	}
	self.errors.PushBack(e)
}

func NewCGenerator() *CGenerator {
	codeFuncTable := make(map[GramCode]func(*Gen, Gram))
	generator := new(CGenerator)
	*generator = CGenerator {
		Generator: Generator {
			codeFuncTable: codeFuncTable,
			errors: list.New(),
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

func (self *CGenerator) genWalk(parent *Gen, gram Gram, table GenTable) {
	for subgram := range NewGramWalk(gram) {
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

func (self *CGenerator) findTypedef(parent *Gen, name string) {
	
}

func (self *CGenerator) confirmType(parent *Gen, gram Gram) {
	typeName := GramTokenString(GramSearch(gram, GRAM_TOKEN_NAME))
	fmt.Print("confirmType ", typeName, "\n")

	if typeName == "string" {
		self.confirmStringDefinition(parent)
	} else {
		typedefZone := self.confirmTypedefZone(parent)

		typeGen := typedefZone.FindNamed(C_TYPEDEF, typeName)

		if typeGen == nil {
			typeGen = typedefZone.NewGram(C_TYPEDEF, typeName, gram)
			typedefZone.children.PushBack(typeGen)

			moduleGram := GramIterParents(gram).Filter(GRAM_MODULE).First()
			typedefGram := GramFindNamed(moduleGram, GRAM_DEFINE_TYPE, typeName)

			fmt.Print("build typedef ", typedefGram, "\n")

			if typedefGram == nil {
				// Report error.
				self.addError("No type " + typeName, gram)
				return
			}

			fmt.Print("typedef\n")
			typeGen.PushToken("typedef").AddSpace()
			self.genType(typeGen, GramFind(typedefGram, GRAM_TYPE))
			structName := self.structNameFromGrams(
				parent, gram,
			)
			typeGen.AddSpace().PushToken(structName)
			typeGen.PushToken(";").AddLine().AddLine()

			moduleGen := parent.IterParents().Filter(C_SOURCE).First()
			self.confirmImplementations(moduleGen, typedefGram)
		}
	}
}

func (self *CGenerator) confirmArrayType(parent *Gen, gram Gram) {
	typedefZone := self.confirmTypedefZone(parent)

	arrayName := self.arrayTypeName(parent, gram)
	gen := typedefZone.FindNamed(C_TYPEDEF, arrayName)

	if gen == nil {
		// Confirm typedefs for dependees.
		subtypeGram := NewGramWalk(gram).Filter(GRAM_TYPE).Last()
		fmt.Print(GramTokenString(subtypeGram), "\n")
		self.confirmType(parent, subtypeGram)

		axisCount := NewGramWalk(gram).Filter(GRAM_TOKEN_SQUARE_BEGIN).Count();

		gen = parent.NewGram(C_TYPEDEF, arrayName, nil)
		typedefZone.children.PushBack(gen)

		gen.PushToken("typedef").AddSpace()
		gen.PushToken("struct").AddSpace()
		gen.PushToken("{").AddLine()

		indent := gen.IndentBlock()
		if axisCount == 1 {
			line := indent.Line()
			line.PushToken("int").AddSpace()
			line.PushToken("length").PushToken(";")
		} else {
			for i := 0; i < axisCount; i += 1 {
				line := indent.Line()
				line.PushToken("int").AddSpace()
				line.PushToken("length").PushToken(fmt.Sprint(i + 1)).PushToken(";")
			}
		}

		line := indent.Line()
		self.genType(line, subtypeGram)
		line.AddSpace().PushToken("*").PushToken("value").PushToken(";")

		gen.PushToken("}").AddSpace().PushToken(arrayName).PushToken(";")
		gen.AddLine().AddLine()
	}
}

func (self *CGenerator) arrayTypeName(parent *Gen, gram Gram) string {
	s := ""
	for subgram := range NewGramWalk(gram) {
		if subgram.Code() == GRAM_TOKEN_SQUARE_BEGIN {
			s += "_array"
		} else if subgram.Code() == GRAM_TOKEN_NAME {
			s = self.structNameFromGrams(parent, gram) + s
		}
	}
	return s
}

func (self *CGenerator) genStructTypeMember(parent *Gen, gram Gram) {
	gen := parent.NewGram(C_STRUCT_MEMBER, "", gram)
	parent.children.PushBack(gen)

	typeGram := GramSearch(gram, GRAM_TYPE)

	members := 0
	for nameGram := range NewGramIter(gram).Filter(GRAM_TOKEN_NAME) {
		members += 1
		gen.AddIndent()
		self.genType(gen, typeGram)
		gen.AddSpace().PushToken(GramTokenString(nameGram)).PushToken(";")
		gen.AddLine()
	}

	if members == 0 {
		
	}
}

func (self *CGenerator) genStructType(parent *Gen, gram Gram) {
	gen := parent.NewGram(C_STRUCT, "", gram)
	parent.children.PushBack(gen)

	gen.PushToken("struct").AddSpace().PushToken("{").AddLine()
	indent := gen.IndentBlock()
	for member := range NewGramIter(gram).Filter(GRAM_OBJECT_MEMBER) {
		self.genStructTypeMember(indent, member)
	}
	gen.AddIndent().PushToken("}")
}

func (self *CGenerator) genType(parent *Gen, gram Gram) {
	gen := parent.NewGram(C_TYPE, "", gram)
	parent.children.PushBack(gen)

	if gram == nil {
		gen.PushToken("void")
		return
	}

	for subgram := range NewGramIter(gram) {
		if subgram.Code() == GRAM_TOKEN_NAME {
			typeName := GramTokenString(subgram)
			if typeName == "int" {
				gen.PushToken(typeName)
			} else if typeName == "real" {
				gen.PushToken("double")
			} else {
				self.confirmType(parent, gram)
				gen.PushToken(self.structNameFromGrams(parent, gram))
			}
		} else if subgram.Code() == GRAM_TOKEN_ASTERICK {
			self.genType(gen, GramFind(gram, GRAM_TYPE))
			gen.AddSpace().PushToken("*")
			break
		} else if subgram.Code() == GRAM_TOKEN_SQUARE_BEGIN {
			// self.genType(gen, GramFind(gram, GRAM_TYPE))
			self.confirmArrayType(parent, gram)
			gen.PushToken(self.arrayTypeName(parent, gram))
			// gen.PushToken("_array")
			// gen.AddSpace().PushToken("*")
			break
		} else if subgram.Code() == GRAM_OBJECT_TYPE {
			self.genStructType(gen, subgram)
		} else if subgram.Code() == GRAM_TYPE {
			self.genType(gen, subgram)
		}
	}
}

func (self *CGenerator) genObjectLiteral(parent *Gen, gram Gram) {
	gen := parent.NewGram(C_LITERAL_OBJECT, "", gram)
	parent.children.PushBack(gen)

	typeGram := GramFind(gram, GRAM_TYPE)

	self.confirmType(parent, typeGram)

	gen.PushToken("(")
	self.genType(gen, typeGram)
	gen.PushToken(")").AddSpace()

	gen.PushToken("{").AddLine()
	indent := gen.IndentBlock()
	if namedMember := GramFind(gram, GRAM_LITERAL_NAMED_MEMBER_NODE);
		namedMember != nil {
		// typeDef := self.typeDef(parent, typeGram)
		//
		// for definedMember := range GramSearch(gram, GRAM_LITERAL_NAMED_MEMBER_NODE) {
		// 	// Lookup field in literal and put it here. Else put a default value.
		// }
	} else {
		self.genWalk(indent, gram, GenTable {
			GRAM_LITERAL_MEMBER: func(parent *Gen, gram Gram) {
				lastLine := parent.Iter().Filter(C_LINE).Last()
				if lastLine != nil {
					lastLine.Find(C_NODE).PushToken(",")
				}

				line := parent.Line()
				gen := line.NewGram(C_LITERAL_MEMBER, "", gram)
				line.children.PushBack(gen)
				self.genExpr(line, GramFind(gram, GRAM_EXPR))
			},
		})
	}
	gen.AddIndent().PushToken("}")
}

func (self *CGenerator) genLiteral(parent *Gen, gram Gram) {
	self.genFirst(parent, gram, GenTable {
		GRAM_TOKEN_INT: func(parent *Gen, gram Gram) {
			gen := parent.NewGram(C_LITERAL_INT, "", gram)
			parent.children.PushBack(gen)
			gen.PushToken(GramTokenString(gram))
		},
		GRAM_TOKEN_REAL: func(parent *Gen, gram Gram) {
			gen := parent.NewGram(C_LITERAL_REAL, "", gram)
			parent.children.PushBack(gen)
			gen.PushToken(GramTokenString(gram))
		},
		GRAM_LITERAL_OBJECT: self.genObjectLiteral,
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

	self.genFirst(gen, gram, GenTable {
		GRAM_EXPR: func(parent *Gen, gram Gram) {
			parent.AddSpace()
			self.genExpr(parent, gram)
		},
		GRAM_LITERAL: func(parent *Gen, gram Gram) {
			parent.AddSpace()
			self.genLiteral(parent, gram)
		},
	})

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
	fmt.Print("genFuncArg\n")
	typeGram := GramFind(gram, GRAM_TYPE)

	self.genAll(parent, gram, GenTable {
		GRAM_TOKEN_NAME: func(parent *Gen, tokenGram Gram) {
			gen := parent.NewGram(C_FUNC_ARG, GramTokenString(tokenGram), gram)
			parent.children.PushBack(gen)

			if parent.children.Len() > 1 {
				gen.PushToken(",").AddSpace()
			}

			self.genType(gen, typeGram)
			gen.AddSpace()
			gen.PushToken(GramTokenString(tokenGram))
		},
	})
}

// Confirm that runtime definitions are available.
func (self *CGenerator) confirmRuntimeHeader(parent *Gen) *Gen {
	return self.confirmHeader(parent, "runtime.h")
}

func (self *CGenerator) confirmStringDefinition(parent *Gen) {
	typedefZone := self.confirmTypedefZone(parent)

	stringName := self.structName("", "string")
	gen := typedefZone.FindNamed(C_TYPEDEF, stringName)

	if gen == nil {
		self.genStringDefinition(typedefZone)
	}
}

func (self *CGenerator) genStringDefinition(parent *Gen) {
	stringName := self.structName("", "string")
	fmt.Printf("%s\n", stringName)
	gen := parent.NewGram(C_TYPEDEF, stringName, nil)
	parent.children.PushBack(gen)

	gen.PushToken("typedef struct {\n")
	gen.PushToken("  int length;\n")
	gen.PushToken("  char *value;\n")
	gen.PushToken("} ").PushToken(stringName).PushToken(";").AddLine().AddLine()
}

func (self *CGenerator) confirmHeader(parent *Gen, name string) *Gen {
	gen := parent.Root().FindNamed(C_HEADER, name)
	if gen == nil {
		gen = self.genHeader(parent.Root(), name)
	}
	return gen
}

func (self *CGenerator) genHeader(parent *Gen, name string) *Gen {
	gen := parent.NewGram(C_HEADER, name, nil)
	parent.children.PushBack(gen)

	onceStartGen := gen.NewGram(C_HEADER_START, "", nil)
	gen.children.PushBack(onceStartGen)
	headerId := strings.ToUpper(strings.Replace(name, ".", "_", -1))
	onceStartGen.PushToken("#ifndef").AddSpace()
	onceStartGen.PushToken(headerId).AddLine().AddLine()
	onceEndGen := gen.NewGram(C_HEADER_END, "", nil)
	gen.children.PushBack(onceEndGen)
	onceEndGen.PushToken("#endif").AddLine()

	return gen
}

// Called with each generate func, check for any interfaces it now implements
// that it didn't prior. Check that the table struct exists for the new
// interface, write a func to return a filled table with the type name and
// pointers to the methods for that type, and make sure a func exists to cast
// the type with an entry for this interface.
func (self *CGenerator) confirmImplementations(parent *Gen, gram Gram) {
	fmt.Print("confirmImplementations\n")
	localPackageName := GramModuleName(gram)
	// check for cast function for type (pointer to instance, type to cast to)
	packageName := GramTypeModuleName(gram)
	typeName := GramTypeName(gram)
	castFuncName := self.funcName(packageName, typeName, "Cast")
	prototypeZone := self.confirmPrototypeZone(parent)
	typedefZone := parent.FindNamed(C_ZONE, "typedef")

	castPrototypeGen := parent.FindNamed(C_PROTOTYPE, castFuncName)
	if castPrototypeGen == nil {
		castPrototypeGen = prototypeZone.NewGram(C_PROTOTYPE, castFuncName, nil)
		prototypeZone.children.PushBack(castPrototypeGen)

		castPrototypeGen.Line().T("void * ").T(castFuncName).T("(void *);")
	}

	castFuncGen := parent.FindNamed(C_FUNC, castFuncName)
	if packageName == localPackageName && castFuncGen == nil {
		// generate cast function
		castFuncGen = parent.NewGram(C_FUNC, castFuncName, nil)
		parent.children.PushBack(castFuncGen)

		castFuncGen.Line().T("void * ").T(castFuncName).T("(void *target) {")
		indent := castFuncGen.IndentBlock()
		castZone := indent.NewGram(C_ZONE, "casts", nil)
		indent.children.PushBack(castZone)
		indent.Line().T("return 0;")
		castFuncGen.Line().T("}")
		castFuncGen.Line()

		blankInterfaceName := "BlankInterface"

		// confirm struct of interface {}
		blankInterfaceTable := typedefZone.FindNamed(C_TYPEDEF, blankInterfaceName)
		if blankInterfaceTable == nil {
			blankInterfaceTable = typedefZone.NewGram(
				C_TYPEDEF, blankInterfaceName, nil,
			)
			typedefZone.children.PushBack(blankInterfaceTable)
			blankInterfaceTable.Line().T("typedef struct {")
			blankInterfaceIndent := blankInterfaceTable.IndentBlock()
			blankInterfaceIndent.Line().T("void * (*typeCast)(void *);")
			blankInterfaceTable.Line().T("} PH_BlankInterfaceTable;")
			blankInterfaceTable.Line()
		}

		// confirm prototype of interface {} target
		blankTargetPrototype := prototypeZone.FindNamed(
			C_PROTOTYPE, blankInterfaceName,
		)
		if blankTargetPrototype == nil {
			blankTargetPrototype = prototypeZone.NewGram(
				C_PROTOTYPE, blankInterfaceName, nil,
			)
			prototypeZone.children.PushBack(blankTargetPrototype)
			blankTargetPrototype.Line().T("void * ").T(
				self.funcName("", blankInterfaceName, "Target"),
			).T("();")
		}

		// generate function to fill blank interface's table
		blankCastFuncName := self.funcName("", typeName, "PH_BlankInterfaceTable")
		blankCastPrototype := prototypeZone.NewGram(
			C_PROTOTYPE, blankCastFuncName, nil,
		)
		prototypeZone.children.PushBack(blankCastPrototype)
		blankCastPrototype.Line().T("void * ").T(blankCastFuncName).T("();")

		blankCastFunc := parent.NewGram(C_FUNC, blankCastFuncName, nil)
		parent.children.PushBack(blankCastFunc)
		blankCastFunc.Line().T("void * ").T(blankCastFuncName).T("() {")
		blankCastIndent := blankCastFunc.IndentBlock()
		blankCastIndent.Line().T("static PH_BlankInterfaceTable table = (PH_BlankInterfaceTable) {")
		blankCastTableIndent := blankCastIndent.IndentBlock()
		blankCastTableIndent.Line().T(castFuncName)
		blankCastIndent.Line().T("};")
		blankCastIndent.Line().T("return &table;")
		blankCastFunc.Line().T("}")
		blankCastFunc.Line()

		// generate cast to interface {}
		blankCast := castZone.NewGram(C_ZONE, "interface {}", nil)
		castZone.children.PushBack(blankCast)
		blankCast.Line().T("if (target == PH_BlankInterface_Target()) {")
		blankIndent := blankCast.IndentBlock()
		blankIndent.Line().T("return ").T(blankCastFuncName).T("();")
		blankCast.Line().T("}")
	}

	// iterate all interfaces
	fmt.Printf("iterate all interfaces\n")
	// moduleIter := NewGramIter(GramRoot(gram)).Filter(GRAM_MODULE)
	for module := range GramIterModules(gram) {
		fmt.Print(module.Code(), " ", GramName(module), "\n")
	}
	typeIter := GramIterModules(gram).Children().Filter(GRAM_DEFINE_TYPE)
	for definedType := range typeIter {
		fmt.Print(GramTypeFullName(definedType), "\n")
		if GramFind(GramFind(definedType, GRAM_TYPE), GRAM_INTERFACE_TYPE) == nil {
			continue
		}

		interfaceModuleName := GramTypeModuleName(definedType)
		interfaceName := GramTypeName(definedType)
		// interfaceFullName := GramTypeFullName(definedType)

		// check if type implements interface
		// check if implementation has been generated

		// confirm interface table
		interfaceTableName := self.structName(
			interfaceModuleName, interfaceName + "Table",
		)
		interfaceTable := typedefZone.FindNamed(C_TYPEDEF, interfaceTableName)
		if interfaceTable == nil {
			interfaceTable = typedefZone.NewGram(C_TYPEDEF, interfaceTableName, nil)
			typedefZone.children.PushBack(interfaceTable)

			interfaceTable.Line().T("typedef struct {")
			indent := interfaceTable.IndentBlock()
			indent.Line().T("void * (*typeCast)(void *);")
			memberIter := NewGramWalk(definedType).Filter(GRAM_INTERFACE_MEMBER)
			for interfaceMember := range memberIter {
				line := indent.Line()
				returnType := GramFind(interfaceMember, GRAM_TYPE)
				self.genType(line, returnType)
				memberName := GramTokenString(GramFind(
					interfaceMember, GRAM_TOKEN_NAME,
				))
				line.T(" (*").T(memberName).T(")(void *, ")
				firstArg := true
				for arg := range NewGramWalk(interfaceMember).Filter(GRAM_FUNC_ARG) {
					argType := GramFind(arg, GRAM_TYPE)
					for _ = range NewGramIter(arg).Filter(GRAM_TOKEN_NAME) {
						if !firstArg {
							line.T(", ")
						}
						self.genType(line, argType)
						firstArg = false
					}
				}
				line.T(");")
			}
			interfaceTable.Line().T("} ").T(interfaceTableName).T(";")
			interfaceTable.Line()
		}

		// confirm interface target prototype
		interfaceTargetName := self.funcName(
			interfaceModuleName, interfaceName, "Target",
		)
		interfacePrototype := prototypeZone.FindNamed(
			C_PROTOTYPE, interfaceTargetName,
		)
		if interfacePrototype == nil {
			interfacePrototype = prototypeZone.NewGram(
				C_PROTOTYPE, interfaceTargetName, nil,
			)
			prototypeZone.children.PushBack(interfacePrototype)

			interfacePrototype.Line().T("void * ").T(interfaceTargetName).T("();")
		}

		// generate implementation
		// generate specific cast
	}
}

func (self *CGenerator) genCast(parent *Gen, gram Gram) {
	// cast interface to concrete type
		// generate check that interface points to type
		// generate deref of instance from interface
	// cast interface to interface
		// call cast method in interface table with target interface
	// cast concrete type to interface
		// confirm type can cast to interface
		// generate inline cast to interface
}

func (self *CGenerator) genImpliedCast(parent *Gen, gram Gram) {
	// confirm type is concrete
	// confirm type can cast to interface
	// generate
}

// func (self *CGenerator) confirmModuleInit(parent *Gen, gram Gram) *Gen {}

// func (self *CGenerator) confirmLanguageInit(parent *Gen, gram Gram) *Gen {}

func (self *CGenerator) genLanguageDetails(parent *Gen) {
	moduleGen := parent
	if moduleGen.code != C_SOURCE {
		moduleGen = parent.FindParent(C_SOURCE)
	}

	if moduleGen == nil {
		return
	}

	// PH_Cast function
	// TODO: Only needed for custom C code.

	// PH_BlankInterface_Target function
	blankInterfaceName := "BlankInterface"
	funcName := self.funcName("", blankInterfaceName, "Target")
	targetFunc := moduleGen.NewGram(C_FUNC, funcName, nil)
	moduleGen.children.PushBack(targetFunc)

	targetFunc.Line().T("void * ").T(funcName).T("() {")
	indent := targetFunc.IndentBlock()
	indent.Line().T("return \"").T(blankInterfaceName).T("\";")
	targetFunc.Line().T("}")
	targetFunc.Line()
}

// Generate the C main function that will translate int argc, char **argv into
// an array of strings, do other initialization, and call PH_main that has been
// generated by a func called main being generated.
func (self *CGenerator) genMainFunc(parent *Gen, gram Gram) {
	funcGen := parent.NewGram(C_FUNC, "main", nil)
	parent.children.PushBack(funcGen)

	typeGen := parent.NewGram(C_TYPE, "", nil)
	typeGen.PushToken("int")
	funcGen.children.PushBack(typeGen)

	funcGen.AddSpace().PushToken("main").PushToken("(int argc, char ** argv)")
	funcGen.AddSpace().PushToken("{ return 0; }")
	funcGen.AddLine().AddLine()

	self.genLanguageDetails(parent)
}

func (self *CGenerator) funcNameFromGrams(
	parent *Gen, receiverGram, nameGram Gram,
) string {
	receiverType := NewGramWalk(receiverGram).Filter(GRAM_TYPE).Last()
	moduleName := ""
	if receiverType != nil {
		moduleName = GramTypeModuleName(receiverType)
	} else if nameGram.Parent() != nil {
		moduleName = GramModuleName(nameGram)
	}
	fmt.Print(moduleName, GramTokenString(nameGram), "\n")
	// TODO: Find package.
	return self.funcName(
		moduleName,
		// "",
		GramTokenString(GramFind(
			NewGramWalk(receiverGram).Filter(GRAM_TYPE).Last(),
			GRAM_TOKEN_NAME,
		)),
		GramTokenString(nameGram),
	)
}

func (self *CGenerator) funcName(
	prefix, receiver, name string,
) string {
	// Use default package prefix.
	if prefix == "" {
		prefix = "PH"
	}

	funcName := prefix
	if receiver != "" {
		funcName += "_" + receiver
	}
	funcName += "_" + name

	return funcName
}

func (self *CGenerator) structNameFromGrams(parent *Gen, nameGram Gram) string {
	moduleName := GramTypeModuleName(nameGram)
	if moduleName == "" {
		moduleName = GramModuleName(nameGram)
	}
	fmt.Print("structNameFromGrams ", moduleName, " ", GramTokenString(nameGram), "\n")

	return self.structName(
		GramTypeModuleName(nameGram),
		GramTypeName(nameGram),
	)
}

func (self *CGenerator) structName(prefix, name string) string {
	// Use default package prefix.
	if prefix == "" {
		prefix = "PH"
	}

	if name == "string" {
		prefix = "PH"
	}

	structName := prefix
	structName += "_" + name

	return structName
}

func (self *CGenerator) genFuncPrototype(parent *Gen, gram Gram) {
	nameGram := GramFind(gram, GRAM_TOKEN_NAME)
	receiverGram := GramFind(gram, GRAM_FUNC_RECEIVER)
	name := self.funcNameFromGrams(parent, receiverGram, nameGram)

	funcGen := parent.NewGram(C_FUNC, name, gram)
	parent.children.PushBack(funcGen)

	funcReturn := GramFind(gram, GRAM_FUNC_RETURN)
	self.genType(funcGen, funcReturn)
	funcGen.AddSpace()

	funcGen.PushToken(name).PushToken("(")

	funcArgsGen := funcGen.NewGram(C_FUNC_ARGS, "", nil)
	funcGen.children.PushBack(funcArgsGen)

	if receiverGram != nil {
		self.genFuncArg(funcArgsGen, receiverGram)
	}

	for arg := range NewGramIter(gram).Filter(GRAM_FUNC_ARG) {
		self.genFuncArg(funcArgsGen, arg)
	}

	funcGen.PushToken(")").PushToken(";").AddLine()
}

func (self *CGenerator) confirmTypedefZone(parent *Gen) *Gen {
	if parent.code != C_SOURCE {
		parent = parent.IterParents().Filter(C_SOURCE).First()
	}
	gen := parent.FindNamed(C_ZONE, "typedef")

	if gen == nil {
		gen = parent.NewGram(C_ZONE, "typedef", nil)
		typedefZone := parent.children.PushFront(gen)
		lineGen := parent.NewToken(C_TOKEN, "", "\n")
		parent.children.InsertAfter(lineGen, typedefZone)
	}

	return gen
}

func (self *CGenerator) confirmPrototypeZone(parent *Gen) *Gen {
	gen := parent.FindNamed(C_ZONE, "prototype")

	if gen == nil {
		gen = parent.NewGram(C_ZONE, "prototype", nil)
		typedefZone := parent.ElementAt(parent.FindNamed(C_ZONE, "typedef"))
		var prototypeZone *list.Element
		if typedefZone != nil {
			prototypeZone = parent.children.InsertAfter(gen, typedefZone)
		} else {
			prototypeZone = parent.children.PushFront(gen)
		}
		lineGen := parent.NewToken(C_TOKEN, "", "\n")
		parent.children.InsertAfter(lineGen, prototypeZone)
	}

	return gen
}

func (self *CGenerator) genFunc(parent *Gen, gram Gram) {
	nameGram := GramFind(gram, GRAM_TOKEN_NAME)
	receiverGram := GramFind(gram, GRAM_FUNC_RECEIVER)
	name := self.funcNameFromGrams(parent, receiverGram, nameGram)
	fmt.Print("genFunc", name)

	self.genFuncPrototype(self.confirmPrototypeZone(parent), gram)

	if GramTokenString(nameGram) == "main" {
		self.genMainFunc(parent, gram)
	}

	funcGen := parent.NewGram(C_FUNC, name, gram)
	parent.children.PushBack(funcGen)

	funcReturn := GramFind(gram, GRAM_FUNC_RETURN)
	self.genType(funcGen, funcReturn)
	funcGen.AddSpace()

	funcGen.PushToken(name).PushToken("(")

	funcArgsGen := funcGen.NewGram(C_FUNC_ARGS, "", nil)
	funcGen.children.PushBack(funcArgsGen)

	if receiverGram != nil {
		self.genFuncArg(funcArgsGen, receiverGram)
	}

	for arg := range NewGramIter(gram).Filter(GRAM_FUNC_ARG) {
		self.genFuncArg(funcArgsGen, arg)
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
		sourceGen := parent.NewGram(C_SOURCE, fileNameAsSource(name), gram)
		parent.children.PushBack(sourceGen)

		self.genAll(sourceGen, gram, GenTable {
			GRAM_FUNC: self.genFunc,
			GRAM_DEFINE_TYPE: func(parent *Gen, gram Gram) {
				// self.genFirst(parent, gram, GenTable {
				// 	GRAM_TYPE: self.confirmType,
				// })
			},
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
