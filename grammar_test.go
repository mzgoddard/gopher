package main

import (

	"container/list"
	"testing"
)

func TestMatchGrammar(t *testing.T) {
	// grams := gramCodesToList([]GramCode {
	// 	GRAM_TOKEN_NAME,
	// })
	// target := []interface {} {
	// 	GRAM_TOKEN_NAME,
	// }
	// if _, match := matchGramsElement(grams.Front(), target); !match {
	// 	t.Error("NAME matches NAME")
	// }
	// // if nextE, _ := matchGramsElement(grams.Front(), target); nextE == nil {
	// // 	t.Error("NAME matches NAME")
	// // }
	// if !matchGrams(grams, target) {
	// 	t.Error("NAME matches NAME")
	// }
	// if !matchGrams(grams, GRAM_TOKEN_NAME) {
	// 	t.Error("NAME matches NAME")
	// }
	// grams = gramCodesToList([]GramCode {
	// 	GRAM_TOKEN_NAME,
	// })
	// target = []interface {} {
	// 	GRAM_TOKEN_FUNC,
	// }
	// if matchGrams(grams, target) {
	// 	t.Error("NAME doesn't match FUNC")
	// }
	// // func name() {}
	// grams = gramCodesToList([]GramCode {
	// 	GRAM_TOKEN_FUNC, GRAM_TOKEN_WS,
	// 	GRAM_TOKEN_NAME, GRAM_TOKEN_PARAN_BEGIN, GRAM_TOKEN_PARAN_END,
	// 	GRAM_TOKEN_WS, GRAM_TOKEN_CURLY_BEGIN, GRAM_TOKEN_CURLY_END,
	// })
	// target = []interface {} {
	// 	GRAM_TOKEN_FUNC, GRAM_TOKEN_WS,
	// 	GRAM_TOKEN_NAME, GRAM_TOKEN_PARAN_BEGIN, GRAM_TOKEN_PARAN_END,
	// 	GRAM_TOKEN_WS, GRAM_TOKEN_CURLY_BEGIN, GRAM_TOKEN_CURLY_END,
	// }
	// if !matchGrams(grams, target) {
	// 	t.Error("many tokens matches many tokens")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("func name() {}"))
	// if !matchGrams(grams, target) {
	// 	t.Error("Parsed func declartion matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("object {}"))
	// if _, match := matchGramObjectType(grams.Front()); !match {
	// 	t.Error("Parsed object type matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("object {\n  name Type\n}"))
	// // t.Log(codesAsNames(tokensCodeArray(ParseAllTokens(
	// // 	"object {\n  name, word Type}"
	// // ))))
	// if _, match := matchGramObjectType(grams.Front()); !match {
	// 	t.Error("Parsed object type with 1 member matches")
	// }
	//
	// if _, match := matchGramType(grams.Front()); !match {
	// 	t.Error("Parsed object type with 1 member matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("interface {}"))
	// if _, match := matchGramInterfaceType(grams.Front()); !match {
	// 	t.Error("Parsed interface type matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("interface {\n  String()\n}"))
	// if _, match := matchGramInterfaceType(grams.Front()); !match {
	// 	t.Error("Parsed interface type with 1 member matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens(
	// 	"interface {\n  String(a, b X) string\n}",
	// ))
	// if _, match := matchGramInterfaceType(grams.Front()); !match {
	// 	t.Error("Parsed interface type with 1 member matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("func()"))
	// if _, match := matchGramFuncType(grams.Front()); !match {
	// 	t.Error("Parsed func type matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("func(a Char) Type"))
	// if _, match := matchGramFuncType(grams.Front()); !match {
	// 	t.Error("Parsed func type with args and return matches")
	// }
	//
	// grams = tokensToGrams(ParseAllTokens("func(a, b Char, c, d X) Type"))
	// if _, match := matchGramFuncType(grams.Front()); !match {
	// 	t.Error("Parsed func type with args and return matches")
	// }
	//
	// input := "func(a, b Char, c, d X,\n) Type"
	// grams = tokensToGrams(ParseAllTokens(input))
	// if _, match := matchGramFuncType(grams.Front()); !match {
	// 	t.Error("Parsed func type with args and return matches")
	// }
	//
	// input = "func(\n  a, b Char,\n  c, d X,\n) Type"
	// grams = tokensToGrams(ParseAllTokens(input))
	// if _, match := matchGramFuncType(grams.Front()); !match {
	// 	t.Error("Parsed func type with args and return matches")
	// }
	//
	// if _, ok := parseGrams("[]input", nil, MatchType); !ok {
	// 	t.Error("Matches array type.")
	// }
	//
	// if grams, ok := parseFromString("[]input{}", CaptureType); ok {
	// 	if grams.Len() != 4 {
	// 		t.Error("Capture leaves 4 tokens.")
	// 	}
	// 	if _, ok := parseGrams(grams, grams.Front(), GRAM_TYPE); !ok {
	// 		t.Error("Captures array type.")
	// 	}
	// 	if nextGram, _ := parseGrams(grams, grams.Front(), GRAM_TYPE);
	// 		nextGram == nil {
	// 		t.Error("Capture points at next element.")
	// 	}
	// 	if !checkGram(grams.Front(), []interface {} {
	// 		GRAM_TOKEN_SQUARE_BEGIN, GRAM_TOKEN_SQUARE_END, GRAM_TYPE,
	// 	}) {
	// 		t.Error("Capture contains array type.")
	// 	}
	// } else {
	// 	t.Error("Captures array type.")
	// }
	//
	// if _, ok := parseGrams("input{}", nil, []interface {} {
	// 	Capture { []interface {} {
	// 		CaptureType, GRAM_TOKEN_CURLY_BEGIN, GRAM_TOKEN_CURLY_END,
	// 	}, GRAM_LITERAL_OBJECT },
	// 	}); !ok {
	// 	t.Error("Captures custom literal.")
	// }

	testCapture(t, "123", "literal", CaptureLiteral)
	testCapture(t, "\"abc\"", "literal", CaptureLiteral)
	testCapture(t, "input{}", "literal", CaptureLiteral)
	testCapture(t, "{}", "literal", CaptureLiteral)
	testCapture(t, "input {}", "literal", CaptureLiteral)
	testCapture(t, "input { }", "literal", CaptureLiteral)
	testCapture(t, "123", "expr", CaptureExpr)
	confirmCapture(t, "1", "literal member", CaptureLiteralMember,
		[]interface {} {
			GRAM_LITERAL_MEMBER_NODE,
		},
	)
	testCapture(t, "1,", "literal member", CaptureLiteralMember)
	testCapture(t, "1,2", "literal member", CaptureLiteralMember)
	testCapture(t, "1, 2", "literal member", CaptureLiteralMember)
	testCapture(t, "1, 2 ", "literal member", CaptureLiteralMember)
	testCapture(t, "1,2,", "literal member", CaptureLiteralMember)
	testCapture(t, "1, 2, ", "literal member", CaptureLiteralMember)
	testCapture(t, "1", "literal member", []interface {} {
		CaptureExpr, MatchMaybe { []interface {} { GRAM_TOKEN_COMMA, MatchLines }},
	})
	testCapture(t, "input {1}", "literal", CaptureLiteral)
	testCapture(t, "input { 1 }", "literal", CaptureLiteral)
	testCapture(t, "input { 1, }", "literal", CaptureLiteral)
	testCapture(t, "input {\n1,\n}", "literal", CaptureLiteral)
	testCapture(t, "input {1,2}", "literal", CaptureLiteral)
	testCapture(t, "input { 1, 2 }", "literal", CaptureLiteral)
	testCapture(t, "input { 1, 2, }", "literal", CaptureLiteral)
	testCapture(t, "input { 1,\n 2,\n }", "literal", CaptureLiteral)
	dontCapture(t, "input { 1,\n 2\n }", "literal", CaptureLiteral)

	testCapture(t, "{ 1, 2 }", "literal", CaptureLiteral)
	testCapture(t, "{ 1, 2, }", "literal", CaptureLiteral)

	testCapture(t, "a:1", "custom", []interface {} {
		GRAM_TOKEN_NAME, GRAM_TOKEN_COLON, GRAM_TOKEN_INT,
	})
	testCapture(t, "input { a: 1, b: 2 }", "literal", CaptureLiteral)
	testCapture(t, "input { a: 1, b: 2, }", "literal", CaptureLiteral)

	// if matchGrams()
}

func testCapture(t *testing.T, input, name string, target interface {}) {
	if _, ok := parseGrams(input, nil, target); !ok {
		t.Errorf("Captures %s. Input: %s", name, input)
	}
}

func confirmCapture(
	t *testing.T, input, name string, target, confirm interface {}) {
	grams := parseToGrams(input)
	if _, ok := parseGrams(grams, grams.Front(), target); !ok {
		t.Errorf("Captures %s. Input: %s", name, input)
	} else {
		if _, ok := parseGrams(grams, grams.Front(), confirm); !ok {
			t.Errorf("Confirms %s. Input: %s", name, input)
		}
	}
}

func dontCapture(t *testing.T, input, result string, target interface {}) {
	if _, ok := parseGrams(input, nil, target); ok {
		t.Errorf("Doesn't capture %s. Input: %s", result, input)
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
		if _, ok := parseGrams(children, children.Front(), target); ok {
			return ok
		}
	}
	return false
}

func parseFromString(input string, target interface {}) (*list.List, bool) {
	grams := parseToGrams(input)
	_, ok := parseGrams(grams, grams.Front(), target)
	return grams, ok
}
