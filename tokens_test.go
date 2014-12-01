package main

import (
	"container/list"
	"testing"
)

func TestParseToken(t *testing.T) {
	if ParseToken(" \t ").code != TOKEN_WS {
		t.Error("\" \\t \" == TOKEN_WS")
	}
	if ParseToken("\t").code != TOKEN_WS {
		t.Error("\"\\t\" == TOKEN_WS,", '\t')
	}
	if ParseToken(" \t ").String() != " \t " {
		t.Errorf("\" \\t \" == \" \\t \", %d", len(ParseToken(" \t ").String()))
	}
	if ParseToken("\n").code != TOKEN_NEWLINE {
		t.Error("\\n == TOKEN_NEWLINE")
	}
	if ParseToken("+").code != TOKEN_PLUS {
		t.Error("+ == TOKEN_PLUS")
	}
	if ParseToken("-").code != TOKEN_MINUS {
		t.Error("- == TOKEN_MINUS")
	}
	if ParseToken("func").code != TOKEN_FUNC {
		t.Error("func == TOKEN_FUNC")
	}
	if ParseToken("type").code != TOKEN_TYPE {
		t.Error("type == TOKEN_TYPE")
	}
	if ParseToken("123").code != TOKEN_INT {
		t.Error("123 == TOKEN_INT")
	}
	if ParseToken("-123").code != TOKEN_INT {
		t.Error("-123 == TOKEN_INT")
	}
	if ParseToken("word").code != TOKEN_NAME {
		t.Error("word == TOKEN_NAME")
	}
	if ParseToken("word").String() != "word" {
		t.Error("word == word")
	}
	if ParseToken("WORD").code != TOKEN_NAME {
		t.Error("WORD == TOKEN_NAME")
	}
	if ParseToken("WORD").String() != "WORD" {
		t.Error("WORD == WORD")
	}
	if ParseToken("w_rd").code != TOKEN_NAME {
		t.Error("w_rd == TOKEN_NAME")
	}
	if ParseToken("w_rd").String() != "w_rd" {
		t.Error("w_rd == w_rd")
	}
	if ParseToken("'a'").code != TOKEN_CHAR {
		t.Error("'a' == TOKEN_CHAR")
	}
	if ParseToken("'a'").String() != "'a'" {
		t.Error("'a' == 'a'")
	}
	if ParseToken("'\\''").code != TOKEN_CHAR {
		t.Error("'\\'' == TOKEN_CHAR")
	}
	if ParseToken("'\\''").String() != "'\\''" {
		t.Error("'\\'' == '\\''")
	}
	if ParseToken("\"\"").code != TOKEN_STRING {
		t.Error("\"\" == TOKEN_STRING")
	}
	if ParseToken("\"abc\"").code != TOKEN_STRING {
		t.Error("\"abc\" == TOKEN_STRING")
	}
	if ParseToken("\"abc\"").String() != "\"abc\"" {
		t.Error("\"abc\" == \"abc\"")
	}
	if ParseToken("\"\\\"\"").code != TOKEN_STRING {
		t.Error("\"\\\"\" == TOKEN_STRING")
	}
	if ParseToken("\"\\\"\"").String() != "\"\\\"\"" {
		t.Error("\"\\\"\" == \"\\\"\"")
	}
}

func TestParseAllTokens(t *testing.T) {
	tokens := []TokenCode {
		TOKEN_FUNC, TOKEN_WS,
		TOKEN_NAME, TOKEN_PARAN_BEGIN, TOKEN_PARAN_END, TOKEN_WS,
		TOKEN_CURLY_BEGIN, TOKEN_CURLY_END,
	}
	if matchCodes(ParseAllTokens("func name() {}"), tokens) {
		t.Errorf("func name() {} == %s", codesAsNames(tokens))
	}

	tokens = []TokenCode {
		TOKEN_NAME, TOKEN_WS, TOKEN_PLUS, TOKEN_WS, TOKEN_NAME,
	}
	if matchCodes(ParseAllTokens("a + b"), tokens) {
		t.Errorf("a + b == %s", codesAsNames(tokens))
	}

	tokens = []TokenCode {
		TOKEN_NAME, TOKEN_WS, TOKEN_PLUS, TOKEN_WS, TOKEN_NAME,
	}
	if matchCodes(ParseAllTokens("type Number int"), tokens) {
		t.Errorf("type Number int == %s", codesAsNames(tokens))
	}

	tokens = []TokenCode {
		TOKEN_TYPE, TOKEN_WS, TOKEN_NAME, TOKEN_WS, TOKEN_OBJECT, TOKEN_WS,
		TOKEN_CURLY_BEGIN, TOKEN_CURLY_END,
	}
	if matchCodes(ParseAllTokens("type Number object {}"), tokens) {
		t.Errorf("type Number object {} == %s", codesAsNames(tokens))
	}
}

func matchCodes(l *list.List, tokens []TokenCode) bool {
	i := 0
	for e := l.Front(); e != nil; e = e.Next() {
		token, ok := e.Value.(*Token)
		if ok && token.code != tokens[i] {
			return false
		}
		i++
	}
	return true
}

func codesAsNames(tokens []TokenCode) string {
	s := ""
	for i := 0; i < len(tokens); i++ {
		s += tokens[i].Name()
		if i < len(tokens) - 1 {
			s += " "
		}
	}
	return s
}
