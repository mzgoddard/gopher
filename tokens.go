package main

import "container/list"

type TokenCode int

type Token struct {
	code TokenCode
	line, column, index, length int
	source string
}

type Lexer struct {
	index, line, column int
	source string
}

const (
	TOKEN_NONE = iota
	TOKEN_EOF
	TOKEN_WS
	TOKEN_NEWLINE
	TOKEN_PLUS
	TOKEN_MINUS
	TOKEN_ASTERICK
	TOKEN_SLASH
	TOKEN_AMPERSAND
	TOKEN_PIPE
	TOKEN_TILDE
	TOKEN_EXCLAMATION
	TOKEN_EQUAL
	TOKEN_LT
	TOKEN_GT
	TOKEN_NOT
	TOKEN_CAROT
	TOKEN_DOT
	TOKEN_COMMA
	TOKEN_COLON
	TOKEN_SEMICOLON
	TOKEN_PARAN_BEGIN
	TOKEN_PARAN_END
	TOKEN_CURLY_BEGIN
	TOKEN_CURLY_END
	TOKEN_SQUARE_BEGIN
	TOKEN_SQUARE_END
	TOKEN_VAR
	TOKEN_FUNC
	TOKEN_TYPE
	TOKEN_OBJECT
	TOKEN_INTERFACE
	TOKEN_LOOP
	TOKEN_BREAK
	TOKEN_CONTINUE
	TOKEN_IF
	TOKEN_ELSE
	TOKEN_RETURN
	TOKEN_PACKAGE
	TOKEN_IMPORT
	TOKEN_NULL
	TOKEN_INT
	TOKEN_REAL
	TOKEN_CHAR
	TOKEN_STRING
	TOKEN_NAME
)

func (self *Lexer) matchEOF(token *Token) bool {
	if self.index == len(self.source) {
		token.code = TOKEN_EOF
	}
	return token.code != TOKEN_NONE
}

func (self *Lexer) matchWS(token *Token) bool {
	c := self.source[self.index]
	for ; c == ' ' || c == '\t'; c = self.source[self.index] {
		self.index++
		self.column++
		token.length++
		token.code = TOKEN_WS
		if self.index == len(self.source) {
			break
		}
	}
	return token.length > 0
}

func (self *Lexer) matchNewline(token *Token) bool {
	c := self.source[self.index]
	if c == '\n' {
		self.index++
		self.line++
		self.column = 0
		token.length++
		token.code = TOKEN_NEWLINE
	}
	return token.length > 0
}

func (self *Lexer) matchChar(token *Token, char uint8, code TokenCode) bool {
	c := self.source[self.index]
	if c == char {
		self.index++
		self.column++
		token.length++
		token.code = code
	}
	return token.length > 0
}

func (self *Lexer) matchWord(token *Token, word string, code TokenCode) bool {
	index := self.index
	wordIndex := 0
	c := self.source[index]
	for wordIndex < len(word) && c == word[wordIndex] {
		index++
		wordIndex++
		if index < len(self.source) {
			c = self.source[index]
		} else {
			break
		}
	}
	if wordIndex == len(word) {
		token.length = wordIndex
		self.column += token.length
		self.index = index
		token.code = code
	}
	return token.length > 0
}

func (self *Lexer) matchInt(token *Token) bool {
	index := self.index
	c := self.source[index]
	isInt := false
	if c == '-' {
		index++
		if index < len(self.source) {
			c = self.source[index]
		}
	}
	for c >= '0' && c <= '9' {
		isInt = true
		index++
		if index < len(self.source) {
			c = self.source[index]
		} else {
			break
		}
	}
	if isInt {
		token.length = index - token.index
		self.column += token.length
		self.index = index
		token.code = TOKEN_INT
	}
	return token.length > 0
}

func (self *Lexer) matchReal(token *Token) bool { return false }

func (self *Lexer) matchCharLiteral(token *Token) bool {
	index := self.index
	c := self.source[index]
	if c == '\'' {
		index++
		if index < len(self.source) {
			c = self.source[index]
		} else {
			return false
		}
	}
	if c == '\\' {
		index += 2
		if index < len(self.source) {
			c = self.source[index]
		} else {
			return false
		}
	} else {
		index++
		if index < len(self.source) {
			c = self.source[index]
		} else {
			return false
		}
	}
	if c == '\'' {
		index++
		token.length = index - self.index
		self.column += token.length
		self.index = index
		token.code = TOKEN_CHAR
	}
	return token.length > 0
}

func (self *Lexer) matchString(token *Token) bool {
	index := self.index
	c := self.source[index]
	if c == '"' {
		index++
		if index < len(self.source) {
			c = self.source[index]
		} else {
			return false
		}
	}
	for c != '"' && c != '\n' {
		if c == '\\' {
			index += 2
			if index < len(self.source) {
				c = self.source[index]
			} else {
				break
			}
		} else {
			index++
			if index < len(self.source) {
				c = self.source[index]
			} else {
				break
			}
		}
	}
	if c == '"' {
		index++
		token.length = index - self.index
		self.index = index
		self.column += token.length
		token.code = TOKEN_STRING
	}
	return token.length > 0
}

func (self *Lexer) matchName(token *Token) bool {
	index := self.index
	c := self.source[index]
	isName := false
	for c >= 'a' && c <= 'z' ||
		c >= 'A' && c <= 'Z' ||
		c == '_' ||
		c >= '0' && c <= '9' {
		isName = true
		index++
		if index < len(self.source) {
			c = self.source[index]
		} else {
			break
		}
	}
	if isName {
		token.length = index - token.index
		self.column += token.length
		self.index = index
		token.code = TOKEN_NAME
	}
	return token.length > 0
}

func (self *Lexer) ParseToken() Token {
	var token Token
	token = Token {
		code: TOKEN_NONE,
		line: self.line,
		column: self.column,
		index: self.index,
		length: 0,
		source: self.source,
	}

	if self.matchEOF(&token) {
		return token
	} else if self.matchWS(&token) {
		return token
	} else if self.matchNewline(&token) {
		return token
	} else if self.matchInt(&token) {
		return token
	} else if self.matchChar(&token, '+', TOKEN_PLUS) {
		return token
	} else if self.matchChar(&token, '-', TOKEN_MINUS) {
		return token
	} else if self.matchChar(&token, '*', TOKEN_ASTERICK) {
		return token
	} else if self.matchChar(&token, '/', TOKEN_SLASH) {
		return token
	} else if self.matchChar(&token, '&', TOKEN_AMPERSAND) {
		return token
	} else if self.matchChar(&token, '|', TOKEN_PIPE) {
		return token
	} else if self.matchChar(&token, '~', TOKEN_TILDE) {
		return token
	} else if self.matchChar(&token, '!', TOKEN_EXCLAMATION) {
		return token
	} else if self.matchChar(&token, '=', TOKEN_EQUAL) {
		return token
	} else if self.matchChar(&token, '<', TOKEN_LT) {
		return token
	} else if self.matchChar(&token, '>', TOKEN_GT) {
		return token
	} else if self.matchChar(&token, '!', TOKEN_NOT) {
		return token
	} else if self.matchChar(&token, '^', TOKEN_CAROT) {
		return token
	} else if self.matchChar(&token, '.', TOKEN_DOT) {
		return token
	} else if self.matchChar(&token, ',', TOKEN_COMMA) {
		return token
	} else if self.matchChar(&token, ':', TOKEN_COLON) {
		return token
	} else if self.matchChar(&token, ';', TOKEN_SEMICOLON) {
		return token
	} else if self.matchChar(&token, '{', TOKEN_CURLY_BEGIN) {
		return token
	} else if self.matchChar(&token, '}', TOKEN_CURLY_END) {
		return token
	} else if self.matchChar(&token, '(', TOKEN_PARAN_BEGIN) {
		return token
	} else if self.matchChar(&token, ')', TOKEN_PARAN_END) {
		return token
	} else if self.matchChar(&token, '[', TOKEN_SQUARE_BEGIN) {
		return token
	} else if self.matchChar(&token, ']', TOKEN_SQUARE_END) {
		return token
	} else if self.matchWord(&token, "var", TOKEN_VAR) {
		return token
	} else if self.matchWord(&token, "func", TOKEN_FUNC) {
		return token
	} else if self.matchWord(&token, "type", TOKEN_TYPE) {
		return token
	} else if self.matchWord(&token, "object", TOKEN_OBJECT) {
		return token
	} else if self.matchWord(&token, "interface", TOKEN_INTERFACE) {
		return token
	} else if self.matchWord(&token, "for", TOKEN_LOOP) {
		return token
	} else if self.matchWord(&token, "break", TOKEN_BREAK) {
		return token
	} else if self.matchWord(&token, "continue", TOKEN_CONTINUE) {
		return token
	} else if self.matchWord(&token, "if", TOKEN_IF) {
		return token
	} else if self.matchWord(&token, "else", TOKEN_ELSE) {
		return token
	} else if self.matchWord(&token, "return", TOKEN_RETURN) {
		return token
	} else if self.matchWord(&token, "package", TOKEN_PACKAGE) {
		return token
	} else if self.matchWord(&token, "import", TOKEN_IMPORT) {
		return token
	} else if self.matchWord(&token, "null", TOKEN_NULL) {
		return token
	} else if self.matchCharLiteral(&token) {
		return token
	} else if self.matchString(&token) {
		return token
	} else if self.matchName(&token) {
		return token
	}

	return token
}

func (self *Lexer) ParseAllTokens() *list.List {
	list := list.New()
	token := self.ParseToken()
	for token.code != TOKEN_NONE && token.code != TOKEN_EOF {
		tokenP := new(Token)
		*tokenP = token
		list.PushBack(tokenP)
		token = self.ParseToken()
	}
	if token.code == TOKEN_EOF {
		tokenP := new(Token)
		*tokenP = token
		list.PushBack(tokenP)
	}
	return list
}

// func (self *Lexer)

func NewLexer(source string) *Lexer {
	lexer := new(Lexer)
	*lexer = Lexer {
		index: 0,
		line: 0,
		column: 0,
		source: source,
	}
	return lexer
}

func ParseToken(source string) Token {
	return NewLexer(source).ParseToken()
}

func ParseAllTokens(source string) *list.List {
	return NewLexer(source).ParseAllTokens()
}

func (self Token) String() string {
	return self.source[self.index:self.index + self.length]
}

func (self TokenCode) Name() string {
	if self == TOKEN_WS {
		return "WS"
	} else if self == TOKEN_EOF {
		return "EOF"
	} else if self == TOKEN_NEWLINE {
		return "NEWLINE"
	} else if self == TOKEN_PLUS {
		return "PLUS"
	} else if self == TOKEN_FUNC {
		return "FUNC"
	} else if self == TOKEN_NAME {
		return "NAME"
	} else if self == TOKEN_COMMA {
		return "COMMA"
	} else if self == TOKEN_PARAN_BEGIN {
		return "PARAN_BEGIN"
	} else if self == TOKEN_PARAN_END {
		return "PARAN_END"
	} else if self == TOKEN_CURLY_BEGIN {
		return "CURLY_BEGIN"
	} else if self == TOKEN_CURLY_END {
		return "CURLY_END"
	} else if self == TOKEN_OBJECT {
		return "OBJECT"
	}
	return "UNNAMED"
}
