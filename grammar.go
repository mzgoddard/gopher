package main

import (
	"container/list"
	"fmt"
)

type GramCode int

type Gram interface {
	Code() GramCode
	Parent() Gram
	Children() *list.List
	Parse(grams *list.List)
}

type GramIter chan Gram

type Library struct {}

type Parser struct {
	library *Library
	module Gram
	source string
	lexer *Lexer
	tokens *list.List
	grams *list.List
}

type GramBase struct {
	code GramCode
	name, filePath string
	parent Gram
	children *list.List
}

type GramToken struct {
	GramBase
	token *Token
}

// type GramModule struct {
// 	GramBase
// 	declaredTypes []GramDeclareType
// }

type GramNameType struct {}
type GramPtrType struct {}
type GramArrayType struct {}
type GramFuncType struct {}
type GramObjectType struct {}
type GramInterfaceType struct {}

type GramType struct {
	base GramBase
}

type GramDeclareType struct {
	base GramBase
}

type GramDeref struct {}
type GramCallArg struct {}
type GramCallArgs struct {}
type GramCall struct {}
type GramExpr struct {}
type GramAssign struct {}
type GramReturn struct {}
type GramIf struct {}
type GramLoop struct {}
type GramLoopControl struct {}
type GramStatement struct {}
type GramBody struct {}

type GramFuncArg struct {
	base GramBase
	names []GramToken
	argType Gram
}

type GramFuncArgs struct {
	base GramBase
	args []GramFuncArg
}

type GramFuncSig struct {
	base GramBase
	args GramFuncArgs
	returnType Gram
}

type GramFunc struct {
	base GramBase
	sig GramFuncSig
	body GramBody
}

type GramArrayMember struct {
	GramBase
}

type GramArrayLiteral struct {
	GramBase
}

type GramObjectMember struct {
	GramBase
}

type GramObjectLiteral struct {
	GramBase
}

type GramObjectNamedMember struct {
	GramBase
}

type GramObjectNamedLiteral struct {
	GramBase
}

type GramLiteral struct {
	GramBase
}

const (
	GRAM_NONE = iota
	GRAM_TOKEN_EOF
	GRAM_TOKEN_WS
	GRAM_TOKEN_NEWLINE
	GRAM_TOKEN_PLUS
	GRAM_TOKEN_MINUS
	GRAM_TOKEN_ASTERICK
	GRAM_TOKEN_SLASH
	GRAM_TOKEN_AMPERSAND
	GRAM_TOKEN_PIPE
	GRAM_TOKEN_TILDE
	GRAM_TOKEN_EXCLAMATION
	GRAM_TOKEN_EQUAL
	GRAM_TOKEN_LT
	GRAM_TOKEN_GT
	GRAM_TOKEN_NOT
	GRAM_TOKEN_CAROT
	GRAM_TOKEN_DOT
	GRAM_TOKEN_COMMA
	GRAM_TOKEN_COLON
	GRAM_TOKEN_SEMICOLON
	GRAM_TOKEN_PARAN_BEGIN
	GRAM_TOKEN_PARAN_END
	GRAM_TOKEN_CURLY_BEGIN
	GRAM_TOKEN_CURLY_END
	GRAM_TOKEN_SQUARE_BEGIN
	GRAM_TOKEN_SQUARE_END
	GRAM_TOKEN_VAR
	GRAM_TOKEN_FUNC
	GRAM_TOKEN_TYPE
	GRAM_TOKEN_OBJECT
	GRAM_TOKEN_INTERFACE
	GRAM_TOKEN_LOOP
	GRAM_TOKEN_BREAK
	GRAM_TOKEN_CONTINUE
	GRAM_TOKEN_IF
	GRAM_TOKEN_ELSE
	GRAM_TOKEN_RETURN
	GRAM_TOKEN_PACKAGE
	GRAM_TOKEN_IMPORT
	GRAM_TOKEN_NULL
	GRAM_TOKEN_INT
	GRAM_TOKEN_REAL
	GRAM_TOKEN_CHAR
	GRAM_TOKEN_STRING
	GRAM_TOKEN_NAME
	GRAM_PTR_TYPE
	GRAM_ARRAY_TYPE
	GRAM_FUNC_TYPE
	GRAM_OBJECT_BASE
	GRAM_OBJECT_MEMBER
	GRAM_OBJECT_TYPE
	GRAM_INTERFACE_MEMBER
	GRAM_INTERFACE_TYPE
	GRAM_TYPE
	GRAM_DEFINE_TYPE
	GRAM_LITERAL_MEMBER
	GRAM_LITERAL_NAMED_MEMBER
	GRAM_LITERAL_MEMBER_NODE
	GRAM_LITERAL_NAMED_MEMBER_NODE
	GRAM_LITERAL_OBJECT
	GRAM_LITERAL
	GRAM_OPERAND
	GRAM_OP_LOGICAL_OR
	GRAM_OP_ADD
	GRAM_OP_MUL
	GRAM_OP_UNARY
	GRAM_EXPR
	GRAM_LHS
	GRAM_DEREFERENCE
	GRAM_DECLARE_VARIABLE
	GRAM_AUTO_ASSIGNMENT
	GRAM_ASSIGNMENT
	GRAM_RETURN
	GRAM_CALL_ARG
	GRAM_CALL_ARG_NODE
	GRAM_CALL
	GRAM_CAST
	GRAM_BRANCH_ELSE
	GRAM_BRANCH
	GRAM_LOOP
	GRAM_STATEMENT
	GRAM_FUNC_RECEIVER
	GRAM_FUNC_ARG
	GRAM_FUNC_ARG_NODE
	GRAM_FUNC_RETURN
	GRAM_FUNC
	GRAM_PACKAGE_ID
	GRAM_MODULE
)

func GramRoot(self Gram) Gram {
	root := GramIterParents(self).Last()
	if root == nil {
		root = self
	}
	return root
}

func GramModule(self Gram) Gram {
	if self.Code() == GRAM_MODULE {
		return self
	} else {
		return GramIterParents(self).Filter(GRAM_MODULE).First()
	}
}

func GramIterModules(self Gram) GramIter {
	root := GramRoot(self)
	if root.Code() == GRAM_MODULE {
		c := make(GramIter)
		go func() {
			c <- root
			close(c)
		}()
		return c
	} else {
		return NewGramIter(root).Filter(GRAM_MODULE)
	}
}

func GramFindFunc(self Gram, test func(Gram) bool) Gram {
	return NewGramIter(self).FilterFunc(test).First()
}

func GramFind(self Gram, code GramCode) Gram {
	return NewGramIter(self).Filter(code).First()
}

func GramFindNamed(self Gram, code GramCode, name string) Gram {
	return NewGramIter(self).FilterNamed(code, name).First()
}

func GramSearch(self Gram, code GramCode) Gram {
	return NewGramWalk(self).Filter(code).First()
}

func GramIterParents(self Gram) GramIter {
	c := make(GramIter)
	go func() {
		defer close(c)
		if self == nil || self.Children() == nil {
			return
		}
		for parent := self.Parent(); parent != nil; parent = parent.Parent() {
			c <- parent
		}
	}()
	return c
}

func NewGramIter(self Gram) GramIter {
	c := make(GramIter)
	go func() {
		defer close(c)
		if self == nil {
			return
		}
		if self.Children() == nil {
			return
		}
		for e := self.Children().Front(); e != nil; e = e.Next() {
			if gram, ok := e.Value.(Gram); ok {
				c <- gram
			}
		}
	}()
	return c
}

func NewGramWalk(self Gram) GramIter {
	c := make(GramIter)
	go func() {
		defer close(c)
		for gram := range NewGramIter(self) {
			c <- gram
			for subgram := range NewGramWalk(gram) {
				c <- subgram
			}
		}
	}()
	return c
}

func (self GramIter) Children() GramIter {
	c := make(GramIter)
	go func() {
		defer close(c)
		for gram := range self {
			for subgram := range NewGramIter(gram) {
				c <- subgram
			}
		}
	}()
	return c
}

func (self GramIter) FilterFunc(test func(gram Gram) bool) GramIter {
	c := make(GramIter)
	go func() {
		defer close(c)
		for gram := range self {
			if test(gram) {
				c <- gram
			}
		}
	}()
	return c
}

func (self GramIter) Filter(code GramCode) GramIter {
	return self.FilterFunc(func(gram Gram) bool { return gram.Code() == code })
}

func (self GramIter) FilterNamed(code GramCode, name string) GramIter {
	return self.FilterFunc(func(gram Gram) bool {
		if gramBase, ok := gram.(*GramBase); ok {
			return gram.Code() == code && gramBase.name == name
		}
		return false
	})
}

func (self GramIter) First() Gram {
	if first, ok := <-self; ok {
		return first
	}
	return nil
}

func (self GramIter) Last() Gram {
	var last Gram = nil
	for last = range self {}
	return last
}

func (self GramIter) Count() int {
	i := 0
	for _ = range self {
		i++
	}
	return i
}

func GramName(gram Gram) string {
	if gramBase, ok := gram.(*GramBase); ok {
		return gramBase.name
	}
	return ""
}

func GramTokenString(gram Gram) string {
	if gram == nil {
		return ""
	}
	if gramToken, ok := gram.(*GramToken); ok {
		return gramToken.token.String()
	}
	return ""
}

func GramModuleName(gram Gram) string {
	if gram.Code() == GRAM_MODULE {
		return GramName(gram)
	} else {
		return GramModuleName(GramIterParents(gram).Filter(GRAM_MODULE).First())
	}
}

func GramTypeName(gram Gram) string {
	if gram.Code() == GRAM_TYPE {
		return GramTokenString(NewGramWalk(gram).Filter(GRAM_TOKEN_NAME).Last())
	} else if gram.Code() == GRAM_DEFINE_TYPE {
		return GramTokenString(GramFind(gram, GRAM_TOKEN_NAME))
	}
	return ""
}

func GramTypeModuleName(gram Gram) string {
	if gram.Code() == GRAM_TYPE {
		lastGramType := NewGramWalk(gram).Filter(GRAM_TYPE).Last()
		nameCount := NewGramIter(gram).Filter(GRAM_TYPE).Count()
		if nameCount > 1 {
			return GramTokenString(GramFind(lastGramType, GRAM_TOKEN_NAME))
		} else {
			return GramModuleName(gram)
		}
	} else if gram.Code() == GRAM_DEFINE_TYPE {
		return GramModuleName(gram)
	}
	return "PH"
}

func GramTypeFullName(gram Gram) string {
	return GramTypeModuleName(gram) + "." + GramTypeName(gram)
}

func (self *GramBase) AddChild(g Gram) {
	if gramBase, ok := g.(*GramBase); ok {
		gramBase.parent = self
	}
	if self.children == nil {
		self.children = list.New()
	}
	self.children.PushBack(g)
}

func tokensToGrams(tokens *list.List) *list.List {
	grams := list.New()
	for tokenE := tokens.Front(); tokenE != nil; tokenE = tokenE.Next() {
		if token, ok := tokenE.Value.(*Token); ok {
			grams.PushBack(NewGramToken(token))
		}
	}
	return grams
}

// func (self *Parser) Parse() *Parser {
// 	// lex
// 	self.lexer = NewLexer(self.source)
// 	self.tokens = self.lexer.ParseAllTokens()
//
// 	// turn tokens into grams
// 	self.grams = tokensToGrams(self.tokens)
//
// 	// create module
// 	module := NewGramModule()
// 	self.module = module
//
// 	// parse at module
// 	module.Parse(self.grams)
//
// 	return self
// }

func (self *Parser) Transform(root Gram) {}

func NewParser(source string) *Parser {
	parser := new(Parser)
	*parser = Parser {
		source: source,
	}
	return parser
}

func NewDefaultParser() *Parser {
	parser := new(Parser)
	*parser = Parser {}
	return parser
}

func (self *Library) AddFile(path string) {}

func (self *Library) CollectDeps() {}

func NewLibrary() *Library {
	return nil
}

func NewGramToken(token *Token) *GramToken {
	gram := new(GramToken)
	*gram = GramToken {
		GramBase: GramBase {
			code: GramCode(token.code),
		},
		token: token,
	}
	return gram
}

func (self *GramBase) Code() GramCode {
	return self.code
}

func (self *GramBase) Parent() Gram {
	return self.parent
}

func (self *GramBase) Children() *list.List {
	return self.children
}

func (self *GramBase) Parse(grams *list.List) {}

func (self *GramBase) Transform(gram Gram) {}

// func NewGramModule() *GramModule {
// 	gram := new(GramModule)
// 	*gram = GramModule {
// 		GramBase: GramBase {
// 			code: GRAM_MODULE,
// 		},
// 	}
// 	return gram
// }

// func (self GramModule) Parse(grams *list.List) {}

// func (self *GramLiteral) Parse(grams *list.List) {
// 	if matchGrams(grams, []interface {} { GRAM_TOKEN_INT }) {
// 		if gram, ok := grams.Front().Value.(Gram); ok {
// 			literal := new(GramLiteral)
// 			*literal = GramLiteral {
// 				GramBase: GramBase {
// 					code: GRAM_LITERAL,
// 				},
// 			}
// 			self.AddChild(gram)
// 		}
// 	} else if matchGrams(grams, []interface {} { GRAM_TOKEN_REAL }) {
// 	} else if matchGrams(grams, []interface {} { GRAM_TOKEN_CHAR }) {
// 	} else if matchGrams(grams, []interface {} { GRAM_TOKEN_STRING }) {
// 	}
// }

func ConsumeGramLiteral(grams *list.List) *GramLiteral {
	if gram, ok := grams.Front().Value.(Gram); ok {
		literal := new(GramLiteral)
		*literal = GramLiteral {
			GramBase: GramBase {
				code: GRAM_LITERAL,
			},
		}
		literal.AddChild(gram)
		grams.Remove(grams.Front())
		return literal
	}
	return nil
}

type Capture struct {
	target interface {}
	result GramCode
}

type Match struct {
	target interface {}
}

type MatchMaybe struct {
	target interface {}
}

type MatchAny struct {
	target []interface {}
}

type MatchAnyTimes struct {
	target interface {}
}

type MatchTimes struct {
	min, max int
	target interface {}
}

type MatchThen struct {
	target interface {}
	then interface {}
}

type Matcher interface {
	parse(*Parser, *list.List, *list.Element) (*list.Element, bool)
}

func _parseGrams(
	self *Parser,
	grams *list.List,
	start *list.Element,
	target []interface {}) (*list.Element, bool) {
	i := 0
	e := start
	for ; e != nil && i < len(target); {
		if gram, ok := e.Value.(Gram); ok {
			if gramCode, ok := target[i].(GramCode); ok {
				if gram.Code() == gramCode {
					i++
					e = e.Next()
				} else {
					break
				}
			} else if gramCode, ok := target[i].(int); ok {
				if gram.Code() == GramCode(gramCode) {
					i++
					e = e.Next()
				} else {
					break
				}
			} else if
				matchfn, ok := target[i].(func(
					*Parser, *list.List, *list.Element) (*list.Element, bool));
				ok {
				if nextE, match := matchfn(self, grams, e); match {
					i++
					e = nextE
				} else {
					break
				}
			} else if
				matchfn, ok := target[i].(func(
					*Parser, interface {}, *list.Element) (*list.Element, bool));
				ok {
				if nextE, match := matchfn(self, grams, e); match {
					i++
					e = nextE
				} else {
					break
				}
			} else if gramDo, ok := target[i].(func(*Parser, *GramBase)); ok {
				// fmt.Print("gramDo\n")
				if gramBase, ok := gram.(*GramBase); ok {
					gramDo(self, gramBase)
				}
				i++
				e = e.Next()
			} else if matcher, ok := target[i].(Matcher); ok {
				if nextE, ok := matcher.parse(self, grams, e); ok {
					i++
					e = nextE
				} else {
					break
				}
			} else if capture, ok := target[i].(Capture); ok {
				if last, match := parseGrams(self, grams, e, capture.target); match {
					i++
					e = captureGrams(grams, e, last, capture.result).Next()
				} else {
					break
				}
			} else if match, ok := target[i].(MatchThen); ok {
				fail := true
				if _, ok := parseGrams(self, grams, e, match.target); ok {
					if nextE, ok := parseGrams(self, grams, e, match.then); ok {
						i++
						fail = false
						e = nextE
					}
				}
				if fail {
					break
				}
			} else if match, ok := target[i].(MatchMaybe); ok {
				i++
				if nextE, ok := parseGrams(self, grams, e, match.target); ok {
					e = nextE
				}
			} else if match, ok := target[i].(MatchAnyTimes); ok {
				i++
				nextE, ok := e, true
				for ok {
					if nextE, ok = parseGrams(self, grams, nextE, match.target); ok {
						e = nextE
					}
				}
			} else if match, ok := target[i].(MatchAny); ok {
				nextE, ok := e, false
				for j := 0; j < len(match.target) && !ok; j++ {
					nextE, ok = parseGrams(self, grams, e, match.target[j])
				}
				if ok {
					i++
					e = nextE
				} else {
					break
				}
			} else {
				fmt.Printf("Unable to recognize target %d %s\n", i, target[i])
				break
			}
		} else {
			break
		}
	}

	return e, i == len(target)
}

func parseToGrams(input string) *list.List {
	return tokensToGrams(ParseAllTokens(input))
}

func parseGrams(
	self *Parser, grams interface {}, gramE *list.Element, target interface{},
) (*list.Element, bool) {
	if self == nil {
		return parseGrams(NewDefaultParser(), grams, gramE, target)
	}

	if gramsString, ok := grams.(string); ok {
		tokens := parseToGrams(gramsString)
		return parseGrams(self, tokens, tokens.Front(), target)
	}

	if gramsList, ok := grams.(*list.List); ok {
		if targetArray, ok := target.([]interface {}); ok {
			return _parseGrams(self, gramsList, gramE, targetArray)
		} else {
			return _parseGrams(self, gramsList, gramE, []interface {} { target })
		}
	}

	return nil, false
}

func (self Match) parse(parser *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(parser, grams, start, self.target)
}

func newMatcher(target interface {}) Matcher {
	return Match {
		target,
	}
}

// CaptureMember = MatchAnyTimes { MatchAny {
//   Capture { NAME COLON CaptureExpr, NAMED_MEMBER }
//   Capture { CaptureExpr, MEMBER }
//   Capture { NAMED_MEMBER CaptureMember, NAMED_MEMBER_NODE }
//   Capture { MEMBER CaptureMember, MEMBER_NODE }
// }}

// var CaptureExpr = newMatcher(Capture { MatchAny { []interface {} {
// 	CaptureLiteral,
// }}, GRAM_EXPR })

func MatchLines(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAnyTimes {
		MatchAny { []interface {} {
			GRAM_TOKEN_WS,
			GRAM_TOKEN_NEWLINE,
		}},
	})
}

func MatchWS(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAnyTimes {
		GRAM_TOKEN_WS,
	})
}

func RequireLines(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		GRAM_TOKEN_NEWLINE,
		MatchLines,
	})
}

func CaptureLiteralMember(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAnyTimes {
		MatchAny { []interface {} {
			MatchThen {
				Capture { MatchAny { []interface {} {
					[]interface {} {
						Capture { []interface {} {
							GRAM_TOKEN_NAME,
							GRAM_TOKEN_COLON,
							MatchWS,
							CaptureTopOp,
							GRAM_TOKEN_COMMA,
							MatchLines,
						}, GRAM_LITERAL_NAMED_MEMBER },
						CaptureLiteralMember,
					},
					Capture { []interface {} {
						GRAM_TOKEN_NAME,
						GRAM_TOKEN_COLON,
						MatchWS,
						CaptureTopOp,
						MatchWS,
					}, GRAM_LITERAL_NAMED_MEMBER },
				}}, GRAM_LITERAL_NAMED_MEMBER_NODE },
				func(self *Parser, gramBase *GramBase) {
					fieldName := GramTokenString(GramFind(gramBase, GRAM_TOKEN_NAME))
					gramBase.name = fieldName
				},
			},
			Capture { MatchAny { []interface {} {
				[]interface {} {
					Capture { []interface {} {
						CaptureTopOp,
						GRAM_TOKEN_COMMA,
						MatchLines,
					}, GRAM_LITERAL_MEMBER },
					CaptureLiteralMember,
				},
				Capture { []interface {} {
					CaptureTopOp,
					MatchWS,
				}, GRAM_LITERAL_MEMBER },
			}}, GRAM_LITERAL_MEMBER_NODE },
		}},
	})
}

func MatchFuncTypeArg(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchMaybe { MatchAny { []interface {} {
		[]interface {} {
			GRAM_TOKEN_NAME,
			MatchAnyTimes { []interface {} {
				GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
			}},
			GRAM_TOKEN_WS,
			MatchType,
			GRAM_TOKEN_COMMA,
			MatchLines,
			MatchFuncTypeArg,
		},
		[]interface {} {
			GRAM_TOKEN_NAME,
			MatchAnyTimes { []interface {} {
				GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
			}},
			GRAM_TOKEN_WS,
			MatchType,
			MatchWS,
		},
	}}})
}

func MatchFuncTypeSig(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		GRAM_TOKEN_PARAN_BEGIN,
		MatchLines,
		MatchFuncTypeArg,
		GRAM_TOKEN_PARAN_END,
		MatchWS,
		MatchMaybe { MatchType },
	})
}

func MatchFuncType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		GRAM_TOKEN_FUNC,
		MatchFuncTypeSig,
	})
}

func MatchObjectMember(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAnyTimes { []interface {} {
		GRAM_TOKEN_NAME,
		MatchAnyTimes { []interface {} {
			GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
		}},
		GRAM_TOKEN_WS,
		MatchType,
		RequireLines,
	}})
}

func MatchInterfaceMember(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAnyTimes { []interface {} {
		GRAM_TOKEN_NAME,
		MatchFuncTypeSig,
		RequireLines,
	}})
}

func MatchType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		[]interface {} {
			GRAM_TOKEN_SQUARE_BEGIN,
			GRAM_TOKEN_SQUARE_END,
			MatchType,
		},
		[]interface {} {
			GRAM_TOKEN_ASTERICK,
			MatchType,
		},
		[]interface {} {
			GRAM_TOKEN_OBJECT,
			MatchWS,
			GRAM_TOKEN_CURLY_BEGIN,
			MatchLines,
			MatchObjectMember,
			GRAM_TOKEN_CURLY_END,
		},
		[]interface {} {
			GRAM_TOKEN_INTERFACE,
			MatchWS,
			GRAM_TOKEN_CURLY_BEGIN,
			MatchLines,
			MatchInterfaceMember,
			GRAM_TOKEN_CURLY_END,
		},
		MatchFuncType,
		GRAM_TOKEN_NAME,
	}})
}

func CaptureFuncTypeArg(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchMaybe { MatchAny { []interface {} {
		Capture { []interface {} {
			Capture { []interface {} {
				GRAM_TOKEN_NAME,
				MatchAnyTimes { []interface {} {
					GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
				}},
				GRAM_TOKEN_WS,
				CaptureType,
				GRAM_TOKEN_COMMA,
				MatchLines,
			}, GRAM_FUNC_ARG },
			MatchMaybe { CaptureFuncTypeArg },
		}, GRAM_FUNC_ARG_NODE },
		Capture { []interface {} {
			Capture { []interface {} {
				GRAM_TOKEN_NAME,
				MatchAnyTimes { []interface {} {
					GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
				}},
				GRAM_TOKEN_WS,
				MatchAny { []interface {} { CaptureType, GRAM_TYPE }},
				MatchWS,
			}, GRAM_FUNC_ARG },
		}, GRAM_FUNC_ARG_NODE },
		Capture { []interface {} {
			Capture { []interface {} {
				CaptureType,
				GRAM_TOKEN_COMMA,
				MatchLines,
			}, GRAM_FUNC_ARG },
			MatchMaybe { CaptureFuncTypeArg },
		}, GRAM_FUNC_ARG_NODE },
		Capture { []interface {} {
			Capture { []interface {} {
				MatchAny { []interface {} { CaptureType, GRAM_TYPE }},
				MatchWS,
			}, GRAM_FUNC_ARG },
		}, GRAM_FUNC_ARG_NODE },
	}}})
}

func CaptureFuncTypeSig(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		GRAM_TOKEN_PARAN_BEGIN,
		MatchLines,
		CaptureFuncTypeArg,
		GRAM_TOKEN_PARAN_END,
		MatchWS,
		MatchMaybe { CaptureType },
	})
}

func CaptureFuncType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_FUNC,
		CaptureFuncTypeSig,
	}, GRAM_FUNC_TYPE })
}

func CaptureObjectMember(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		MatchMaybe { Capture { []interface {} {
			GRAM_TOKEN_NAME,
			RequireLines,
		}, GRAM_OBJECT_BASE }},
		MatchAnyTimes { Capture { []interface {} {
			GRAM_TOKEN_NAME,
			MatchAnyTimes { []interface {} {
				GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
			}},
			GRAM_TOKEN_WS,
			CaptureType,
			RequireLines,
		}, GRAM_OBJECT_MEMBER }},
	})
}

func CaptureInterfaceMember(
	self *Parser, grams *list.List, start *list.Element,
) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAnyTimes { Capture {
		[]interface {} {
			GRAM_TOKEN_NAME,
			CaptureFuncTypeSig,
			RequireLines,
		},
		GRAM_INTERFACE_MEMBER,
	}})
}

func CaptureObjectType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_OBJECT,
		MatchWS,
		GRAM_TOKEN_CURLY_BEGIN,
		MatchLines,
		CaptureObjectMember,
		GRAM_TOKEN_CURLY_END,
	}, GRAM_OBJECT_TYPE })
}


func CaptureInterfaceType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_INTERFACE,
		MatchWS,
		GRAM_TOKEN_CURLY_BEGIN,
		MatchLines,
		CaptureInterfaceMember,
		GRAM_TOKEN_CURLY_END,
	}, GRAM_INTERFACE_TYPE })
}

func CaptureType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { MatchAny { []interface {} {
		[]interface {} {
			GRAM_TOKEN_SQUARE_BEGIN,
			GRAM_TOKEN_SQUARE_END,
			CaptureType,
		},
		[]interface {} {
			GRAM_TOKEN_ASTERICK,
			CaptureType,
		},
		CaptureObjectType,
		CaptureInterfaceType,
		CaptureFuncType,
		GRAM_TOKEN_NAME,
	}}, GRAM_TYPE })
}

func CaptureLiteralType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { MatchAny { []interface {} {
		[]interface {} {
			GRAM_TOKEN_SQUARE_BEGIN,
			GRAM_TOKEN_SQUARE_END,
			CaptureType,
		},
		[]interface {} {
			GRAM_TOKEN_ASTERICK,
			CaptureType,
		},
		CaptureObjectType,
		CaptureInterfaceType,
		GRAM_TOKEN_NAME,
	}}, GRAM_TYPE })
}

func CaptureSimpleLiteral(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { MatchAny { []interface {} {
		GRAM_TOKEN_INT,
		GRAM_TOKEN_REAL,
		GRAM_TOKEN_CHAR,
		GRAM_TOKEN_STRING,
	}}, GRAM_LITERAL })
}

func CaptureLiteral(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { MatchAny { []interface {} {
		GRAM_TOKEN_INT,
		GRAM_TOKEN_REAL,
		GRAM_TOKEN_CHAR,
		GRAM_TOKEN_STRING,
		MatchThen { []interface {} {
			MatchType, MatchWS, GRAM_TOKEN_CURLY_BEGIN,
		}, Capture { []interface {} {
			CaptureLiteralType, MatchWS, GRAM_TOKEN_CURLY_BEGIN,
			MatchLines, CaptureLiteralMember,
			GRAM_TOKEN_CURLY_END,
		}, GRAM_LITERAL_OBJECT }},
		// Capture { []interface {} {
		// 	GRAM_TOKEN_CURLY_BEGIN,
		// 	MatchLines, CaptureLiteralMember,
		// 	GRAM_TOKEN_CURLY_END,
		// }, GRAM_LITERAL_OBJECT },
	}}, GRAM_LITERAL })
}

func CaptureReturn(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		GRAM_RETURN,
		Capture { []interface {} {
			GRAM_TOKEN_RETURN,
			MatchMaybe { MatchAny { []interface {} {
				[]interface {} { GRAM_TOKEN_WS, CaptureTopOp },
			}}},
		}, GRAM_RETURN },
	}})
}

func CaptureSimpleExpr(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		GRAM_EXPR,
		Capture { MatchAny { []interface {} {
			CaptureSimpleLiteral,
			CaptureParansExpr,
			GRAM_TOKEN_NAME,
			// GRAM_DEREFERENCE,
			// CaptureDereference,
		}}, GRAM_EXPR },
	}})
}

func CaptureExpr(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		GRAM_EXPR,
		Capture { MatchAny { []interface {} {
			CaptureLiteral,
			CaptureParansExpr,
			GRAM_TOKEN_NAME,
			// GRAM_DEREFERENCE,
			// CaptureDereference,
		}}, GRAM_EXPR },
	}})
}

func CaptureParansExpr(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		GRAM_TOKEN_PARAN_BEGIN, MatchWS,
		CaptureTopOp, MatchWS,
		GRAM_TOKEN_PARAN_END,
	})
}

// var name type
func CaptureDeclare(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_VAR, GRAM_TOKEN_WS, GRAM_TOKEN_NAME, GRAM_TOKEN_WS, CaptureType,
	}, GRAM_DECLARE_VARIABLE })
}

// name := expr
func CaptureAutoAssignment(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_NAME, MatchWS,
		GRAM_TOKEN_COLON, GRAM_TOKEN_EQUAL, MatchWS,
		CaptureTopOp,
	}, GRAM_AUTO_ASSIGNMENT })
}

// declare
// dereference
func CaptureLHS(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		[]interface {} {
			GRAM_TOKEN_ASTERICK,
			CaptureLHS,
		},
		MatchThen {
			GRAM_TOKEN_NAME,
			MatchAny { []interface {} {
				MatchThen {
					[]interface {} {
						GRAM_TOKEN_NAME,
						MatchAny { []interface {} {
							GRAM_TOKEN_DOT, GRAM_TOKEN_PARAN_BEGIN,
						}},
					},
					CaptureDereference,
				},
				GRAM_TOKEN_NAME,
			}},
		},
		CaptureDereference,
		GRAM_EXPR,
	}})
}

// lhs = expr
// lhs += expr
// lhs -= expr
// lhs *= expr
// lhs /= expr
func CaptureAssignment(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		Capture { []interface {} {
			CaptureLHS, MatchWS,
			MatchMaybe { MatchAny { []interface {} {
				GRAM_TOKEN_PLUS,
				GRAM_TOKEN_MINUS,
				GRAM_TOKEN_ASTERICK,
				GRAM_TOKEN_SLASH,
			}}},
			GRAM_TOKEN_EQUAL,
		}, GRAM_LHS },
		MatchWS,
		CaptureTopOp,
	}, GRAM_ASSIGNMENT })
}

// expr[expr]
// expr.name
func CaptureDereference(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		MatchThen {
			Capture { MatchAny { []interface {} {
				[]interface {} {
					CaptureSimpleExpr, GRAM_TOKEN_DOT, GRAM_TOKEN_NAME,
				},
				[]interface {} {
					CaptureSimpleExpr, GRAM_TOKEN_SQUARE_BEGIN,
					CaptureLogicalOrOp, GRAM_TOKEN_SQUARE_END,
				},
				[]interface {} {
					GRAM_DEREFERENCE, GRAM_TOKEN_DOT, GRAM_TOKEN_NAME,
				},
				[]interface {} {
					GRAM_DEREFERENCE, GRAM_TOKEN_SQUARE_BEGIN,
					CaptureLogicalOrOp, GRAM_TOKEN_SQUARE_END,
				},
			}}, GRAM_DEREFERENCE },
			CaptureDereference,
		},
		MatchThen {
			[]interface {} { MatchAny { []interface {} {
				CaptureSimpleExpr, GRAM_DEREFERENCE,
			}}, GRAM_TOKEN_PARAN_BEGIN },
			MatchThen {
				Capture { []interface {} {
					CaptureCall,
					MatchAny { []interface {} {
						[]interface {} { GRAM_TOKEN_DOT, GRAM_TOKEN_NAME },
						[]interface {} {
							GRAM_TOKEN_SQUARE_BEGIN,
							CaptureLogicalOrOp, GRAM_TOKEN_SQUARE_END,
						},
					}},
				}, GRAM_DEREFERENCE },
				CaptureDereference,
			},
		},
		GRAM_DEREFERENCE,
	}})
}

func captureOp(
	self *Parser, grams *list.List, start *list.Element,
	opCode, nextOpCode GramCode,
	nextOp interface {},
	matchOperand []interface {},
) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		Capture { []interface {} {
			nextOp, MatchWS,
			Capture { MatchAny { matchOperand }, GRAM_OPERAND }, MatchWS,
			nextOp,
		}, opCode },
		nextOp,
		nextOpCode,
	}})
}

func MatchAnyOp(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		GRAM_OP_LOGICAL_OR,
		GRAM_OP_ADD,
		GRAM_OP_MUL,
		GRAM_OP_UNARY,
		GRAM_DEREFERENCE,
		GRAM_EXPR,
		GRAM_CALL,
	}})
}

func CaptureTopOp(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		CaptureLogicalOrOp, CaptureLiteral,
	}})
}

// expr || expr
func CaptureLogicalOrOp(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return captureOp(
		self, grams, start,
		GRAM_OP_LOGICAL_OR, GRAM_OP_ADD, CaptureAddOp,
		[]interface {} { []interface {} { GRAM_TOKEN_PIPE, GRAM_TOKEN_PIPE }},
	)
}

// expr + expr
// expr - expr
func CaptureAddOp(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return captureOp(
		self, grams, start,
		GRAM_OP_ADD, GRAM_OP_MUL, CaptureMulOp,
		[]interface {} { GRAM_TOKEN_PLUS, GRAM_TOKEN_MINUS },
	)
}

// expr * expr
// expr / expr
func CaptureMulOp(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return captureOp(
		self, grams, start,
		GRAM_OP_MUL, GRAM_OP_UNARY, CaptureUnaryOp,
		[]interface {} { GRAM_TOKEN_ASTERICK, GRAM_TOKEN_SLASH },
	)
}

func CaptureUnaryOp(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		CaptureCall,
		CaptureDereference,
		Capture { []interface {} {
			Capture { MatchAny { []interface {} {
				GRAM_TOKEN_AMPERSAND,
				GRAM_TOKEN_ASTERICK,
				GRAM_TOKEN_PLUS,
				GRAM_TOKEN_MINUS,
				GRAM_TOKEN_TILDE,
				GRAM_TOKEN_EXCLAMATION,
			}}, GRAM_OPERAND }, MatchWS,
			CaptureUnaryOp,
		}, GRAM_OP_UNARY },
		CaptureExpr,
	}})
}

func CaptureCallArg(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchMaybe { Capture { []interface {} {
		Capture { CaptureLogicalOrOp, GRAM_CALL_ARG },
		MatchAny { []interface {} {
			[]interface {} {
				GRAM_TOKEN_COMMA,
				MatchLines,
				CaptureCallArg,
			},
			MatchWS,
		}},
	}, GRAM_CALL_ARG_NODE }})
}

// unary ( args )
func CaptureCall(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		MatchThen {
			Capture { []interface {} {
				MatchAny { []interface {} {
					GRAM_DEREFERENCE, GRAM_EXPR, CaptureDereference, CaptureExpr,
				}},
				GRAM_TOKEN_PARAN_BEGIN,
				MatchLines,
				CaptureCallArg,
				GRAM_TOKEN_PARAN_END,
			}, GRAM_CALL },
			CaptureCall,
		},
		GRAM_CALL,
	}})
}

// dereference.(type)
func CaptureCast(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		CaptureDereference,
		GRAM_TOKEN_DOT,
		GRAM_TOKEN_PARAN_BEGIN,
		MatchLines,
		CaptureType,
		MatchLines,
		GRAM_TOKEN_PARAN_END,
	}, GRAM_CAST })
}

// if expr { statements }
// if statement; expr { statements }
// branch else branch
// branch else { statements }
func CaptureBranch(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		GRAM_BRANCH,
		Capture { []interface {} {
			GRAM_TOKEN_IF, MatchLines,
			MatchAny { []interface {} {
				[]interface {} {
					MatchMaybe { MatchAny { []interface {} {
						CaptureAutoAssignment, CaptureAssignment,
					}}},
					GRAM_TOKEN_SEMICOLON, MatchLines, CaptureLogicalOrOp,
				},
				CaptureLogicalOrOp,
			}},
			MatchLines,
			CaptureBlock,
			MatchMaybe { MatchAny { []interface {} {
				Capture { []interface {} {
					MatchLines,
					GRAM_TOKEN_ELSE,
					MatchLines,
					CaptureBlock,
				}, GRAM_BRANCH_ELSE },
				[]interface {} {
					MatchLines,
					GRAM_TOKEN_ELSE,
					MatchLines,
					CaptureBranch,
				},
			}}},
		}, GRAM_BRANCH },
	}})
}

// for expr { statements }
// for statement; expr; statement { statements }
func CaptureLoop(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchAny { []interface {} {
		GRAM_LOOP,
		Capture { []interface {} {
			GRAM_TOKEN_LOOP, MatchLines,
			MatchAny { []interface {} {
				[]interface {} {
					MatchMaybe { MatchAny { []interface {} {
						CaptureAutoAssignment, CaptureAssignment,
					}}},
					GRAM_TOKEN_SEMICOLON, MatchLines,
					CaptureLogicalOrOp, GRAM_TOKEN_SEMICOLON, MatchLines,
					MatchMaybe { MatchAny { []interface {} {
						CaptureAutoAssignment, CaptureAssignment,
					}}},
				},
				CaptureLogicalOrOp,
			}},
			MatchLines,
			CaptureBlock,
		}, GRAM_LOOP },
	}})
}

func CaptureBlock(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		GRAM_TOKEN_CURLY_BEGIN,
		MatchLines,
		CaptureStatements,
		GRAM_TOKEN_CURLY_END,
	})
}

func CaptureStatements(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, []interface {} {
		MatchAnyTimes { MatchAny { []interface {} {
			GRAM_STATEMENT,
			Capture { []interface {} {
				MatchAny { []interface {} {
					CaptureReturn,
					CaptureBranch,
					CaptureLoop,
					CaptureAutoAssignment,
					CaptureAssignment,
					CaptureCall,
				}},
				RequireLines,
			}, GRAM_STATEMENT },
		}}},
		MatchMaybe { Capture { []interface {} {
			MatchAny { []interface {} {
				CaptureReturn,
				CaptureBranch,
				CaptureLoop,
				CaptureAutoAssignment,
				CaptureAssignment,
				CaptureCall,
			}},
			MatchWS,
		}, GRAM_STATEMENT }},
	})
}

func CaptureFuncArg(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchMaybe {
		MatchAny { []interface {} {
			[]interface {} {
				Capture { []interface {} {
					GRAM_TOKEN_NAME,
					MatchAnyTimes { []interface {} {
						GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
					}},
					GRAM_TOKEN_WS,
					CaptureType,
					GRAM_TOKEN_COMMA,
					MatchLines,
				}, GRAM_FUNC_ARG },
				MatchMaybe { CaptureFuncArg },
			},
			Capture { []interface {} {
				GRAM_TOKEN_NAME,
				MatchAnyTimes { []interface {} {
					GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
				}},
				GRAM_TOKEN_WS,
				MatchAny { []interface {} { CaptureType, GRAM_TYPE }},
				MatchWS,
			}, GRAM_FUNC_ARG },
		}},
	})
}

func CaptureFunc(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_FUNC, GRAM_TOKEN_WS,
		MatchMaybe { Capture { []interface {} {
			GRAM_TOKEN_PARAN_BEGIN, MatchLines,
			GRAM_TOKEN_NAME, MatchLines,
			CaptureType, MatchLines,
			GRAM_TOKEN_PARAN_END, MatchWS,
		}, GRAM_FUNC_RECEIVER }},
		GRAM_TOKEN_NAME, MatchWS,
		GRAM_TOKEN_PARAN_BEGIN, MatchLines,
		CaptureFuncArg,
		GRAM_TOKEN_PARAN_END, MatchWS,
		MatchMaybe { []interface {} {
			Capture { CaptureType, GRAM_FUNC_RETURN },
			MatchWS,
		}},
		GRAM_TOKEN_CURLY_BEGIN, MatchLines,
		CaptureStatements,
		GRAM_TOKEN_CURLY_END,
	}, GRAM_FUNC })
}

func CaptureDefineType(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchThen {
		Capture { []interface {} {
			GRAM_TOKEN_TYPE, GRAM_TOKEN_WS,
			GRAM_TOKEN_NAME, GRAM_TOKEN_WS,
			CaptureType,
		}, GRAM_DEFINE_TYPE },
		func(self *Parser, gramDefineType *GramBase) {
			typeName := GramTokenString(GramFind(gramDefineType, GRAM_TOKEN_NAME))
			fmt.Print("define type ", typeName, "\n")
			gramDefineType.name = typeName
		},
	})
	// )
}

func CapturePackageId(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, Capture { []interface {} {
		GRAM_TOKEN_PACKAGE, GRAM_TOKEN_WS, GRAM_TOKEN_NAME,
	}, GRAM_PACKAGE_ID })
}

func CaptureModule(self *Parser, grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(self, grams, start, MatchThen { Capture { []interface {} {
		MatchLines,
		CapturePackageId,
		MatchLines,
		MatchAnyTimes { MatchAny { []interface {} {
			[]interface {} { CaptureDefineType, RequireLines },
			[]interface {} { CaptureFunc, RequireLines },
		}}},
	}, GRAM_MODULE }, func(self *Parser, grams *list.List, start *list.Element) (
		*list.Element, bool,
	) {
		if gramModule, ok := start.Value.(*GramBase); ok {
			packageId := GramFind(gramModule, GRAM_PACKAGE_ID)
			packageName := GramTokenString(GramFind(packageId, GRAM_TOKEN_NAME))
			gramModule.name = packageName
		}
		return start.Next(), true
	}})
}

func captureGrams(
	grams *list.List,
	start, last *list.Element,
	code GramCode) *list.Element {

	gram := new(GramBase)
	*gram = GramBase {
		code: code,
	}
	i := 0
	for e := start; e != nil && e != last; e = e.Next() {
		if child, ok := e.Value.(Gram); ok {
			gram.AddChild(child)
			i++
		}
	}
	removeNext := start.Next()
	i--
	for ; i > 0; i-- {
		removeAt := removeNext
		removeNext = removeAt.Next()
		grams.Remove(removeAt)
	}
	start.Value = gram
	return start
}
