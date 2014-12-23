package main

import (

	"container/list"
	"testing"
)

func TestMatchGrammar(t *testing.T) {
	confirmObjectType := confirmFactory(
		t, "object type", CaptureObjectType, GRAM_OBJECT_TYPE,
	)
	confirmObjectType("object {}")
	confirmObjectType("object {\n  name Type\n}")

	confirmInterfaceType := confirmFactory(
		t, "interface type", CaptureInterfaceType, GRAM_INTERFACE_TYPE,
	)
	confirmInterfaceType("interface {}")
	confirmInterfaceType("interface {\n  String()\n}")
	confirmInterfaceType("interface {\n  String(a, b X) string\n}")

	confirmFuncType := confirmFactory(
		t, "func type", CaptureFuncType, GRAM_FUNC_TYPE,
	)
	// dontFunc := dontFactory(t, "func type", CaptureFuncType)
	confirmFuncType("func()")
	confirmFuncType("func(Char)")
	confirmFuncType("func(Char, String)")
	confirmFuncType("func(Char, String) Type")
	confirmFuncType("func(Char, String,)")
	confirmFuncType("func(Char, String,) Type")
	confirmFuncType("func(a Char) Type")
	confirmFuncType("func(a, b Char, c, d X) Type")
	confirmFuncType("func(a, b Char, c, d X,\n) Type")
	confirmFuncType("func(\n  a, b Char,\n  c, d X,\n) Type")
	confirmFuncType("func(a, b Char, X)")
	confirmFuncType("func(Char, a, b X)")

	confirmCapture(t, "[]input", "type", CaptureType, GRAM_TYPE)

	testLiteral := testFactory(t, "literal", CaptureLiteral)
	testLiteral("123")
	testLiteral("\"abc\"")
	testLiteral("input{}")
	testLiteral("{}")
	testLiteral("input {}")
	testLiteral("input { }")

	// testExpr := testFactory(t, "expr", CaptureExpr)
	confirmExpr := confirmFactory(t, "expr", CaptureExpr, []interface {} {
		GRAM_EXPR, GRAM_TOKEN_EOF,
	})
	confirmExpr("123")

	testLiteralMember := testFactory(t, "literal member", CaptureLiteralMember)
	confirmLiteralMember := confirmFactory(
		t, "literal member", CaptureLiteralMember, GRAM_LITERAL_MEMBER_NODE,
	)
	confirmLiteralMember("1")
	testLiteralMember("1,")
	testLiteralMember("1,2")
	testLiteralMember("1, 2")
	testLiteralMember("1,2,")
	testLiteralMember("1, 2, ")
	testCapture(t, "1", "literal member", []interface {} {
		CaptureExpr, MatchMaybe { []interface {} { GRAM_TOKEN_COMMA, MatchLines }},
	})

	testLiteral("input {1}")
	confirmCapture(t, "input {1}", "literal", CaptureLiteral, []interface {} {
		GRAM_LITERAL, GRAM_TOKEN_EOF,
	})
	testLiteral("input { 1 }")
	testLiteral("input { 1, }")
	testLiteral("input {\n1,\n}")
	testLiteral("input {1,2}")
	testLiteral("input { 1, 2 }")
	testLiteral("input { 1, 2, }")
	testLiteral("input { 1,\n 2,\n }")
	dontCapture(t, "input { 1,\n 2\n }", "literal", CaptureLiteral)

	testLiteral("{ 1, 2 }")
	testLiteral("{ 1, 2, }")

	testCapture(t, "a:1", "custom", []interface {} {
		GRAM_TOKEN_NAME, GRAM_TOKEN_COLON, GRAM_TOKEN_INT,
	})
	testLiteral("input { a: 1, b: 2 }")
	testLiteral("input { a: 1, b: 2, }")

	confirmFuncReturn := confirmFactory(t, "func return",
		CaptureReturn, []interface {} {
			GRAM_RETURN, GRAM_TOKEN_EOF,
		},
	)
	confirmFuncReturn("return")
	confirmFuncReturn("return 0")

	confirmFunc := confirmFactory(t, "func", CaptureFunc, []interface {} {
		GRAM_FUNC, GRAM_TOKEN_EOF,
	})
	confirmFunc("func name() {}")
	confirmFunc("func name(a Char) {}")
	confirmFunc("func name(a Char, b Char) {}")
	confirmFunc("func name(a, b Char) {}")
	confirmFunc("func name() {\n  return\n}")
	confirmFunc("func name() int {}")
	confirmFunc("func name() int {\n}")
	// confirmFunc("func name() int {return 0}")
	confirmFunc("func name() int {\n  return 0\n}")

	// if matchGrams()
}

func testCapture(t *testing.T, input, name string, target interface {}) {
	if _, ok := parseGrams(nil, input, nil, target); !ok {
		t.Errorf("Captures %s. Input: %s", name, input)
	}
}

func testFactory(t *testing.T, name string, target interface {},
	) func(input string) {
	return func(input string) {
		testCapture(t, input, name, target)
	}
}

func confirmCapture(
	t *testing.T, input, name string, target, confirm interface {}) {
	grams := parseToGrams(input)
	if _, ok := parseGrams(nil, grams, grams.Front(), target); !ok {
		t.Errorf("Captures %s. Input: %s", name, input)
	} else {
		if _, ok := parseGrams(nil, grams, grams.Front(), confirm); !ok {
			t.Log(grams.Front().Value, grams.Front().Next().Value)
			if gram, ok := grams.Front().Next().Value.(*GramToken); ok {
				t.Log(gram.token.String(), gram.token.line, gram.token.column)
			}
			t.Errorf("Confirms %s. Input: %s", name, input)
		}
	}
}

func confirmFactory(t *testing.T, name string, target, confirm interface{},
	) func(string) {
	return func(input string) {
		confirmCapture(t, input, name, target, confirm)
	}
}

func dontCapture(t *testing.T, input, result string, target interface {}) {
	if _, ok := parseGrams(nil, input, nil, target); ok {
		t.Errorf("Doesn't capture %s. Input: %s", result, input)
	}
}

func dontFactory(t *testing.T, name string, target interface {},
	) func(input string) {
	return func(input string) {
		dontCapture(t, input, name, target)
	}
}

func gramCodesToList(grams []GramCode) *list.List {
	l := list.New()
	for i := 0; i < len(grams); i++ {
		l.PushBack(&GramBase {
			code: grams[i],
		});
	}
	return l
}

func gramListToCodes(grams *list.List) []GramCode {
	a := make([]GramCode, grams.Len())
	i := 0
	for e := grams.Front(); e != nil; e = e.Next() {
		if gram, ok := e.Value.(Gram); ok {
			a[i] = gram.Code()
		}
		i++
	}
	return a
}

func checkGram(gram *list.Element, target interface {}) bool {
	if base, ok := gram.Value.(Gram); ok {
		children := base.Children()
		if _, ok := parseGrams(nil, children, children.Front(), target); ok {
			return ok
		}
	}
	return false
}

func parseFromString(input string, target interface {}) (*list.List, bool) {
	grams := parseToGrams(input)
	_, ok := parseGrams(nil, grams, grams.Front(), target)
	return grams, ok
}
