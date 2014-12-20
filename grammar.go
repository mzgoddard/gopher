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

type Library struct {}

type Parser struct {
	library *Library
	root Gram
	source string
	lexer *Lexer
	tokens *list.List
	grams *list.List
}

type GramBase struct {
	code GramCode
	parent Gram
	children *list.List
}

type GramToken struct {
	GramBase
	token *Token
}

type GramModule struct {
	GramBase
	declaredTypes []GramDeclareType
}

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
	GRAM_TOKEN_DIVIDE
	GRAM_TOKEN_EQUAL
	GRAM_TOKEN_LT
	GRAM_TOKEN_GT
	GRAM_TOKEN_NOT
	GRAM_TOKEN_CAROT
	GRAM_TOKEN_DOT
	GRAM_TOKEN_COMMA
	GRAM_TOKEN_COLON
	GRAM_TOKEN_PARAN_BEGIN
	GRAM_TOKEN_PARAN_END
	GRAM_TOKEN_CURLY_BEGIN
	GRAM_TOKEN_CURLY_END
	GRAM_TOKEN_SQUARE_BEGIN
	GRAM_TOKEN_SQUARE_END
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
	GRAM_DECLARE_TYPE
	GRAM_LITERAL_MEMBER
	GRAM_LITERAL_NAMED_MEMBER
	GRAM_LITERAL_MEMBER_NODE
	GRAM_LITERAL_NAMED_MEMBER_NODE
	GRAM_LITERAL_OBJECT
	GRAM_LITERAL
	GRAM_EXPR
	GRAM_ASSIGN
	GRAM_STATEMENT
	GRAM_FUNC_ARG
	GRAM_FUNC_ARG_NODE
	GRAM_FUNC_RETURN
	GRAM_FUNC
	GRAM_MODULE
)

func GramRoot(self Gram) Gram {
	return nil
}

func GramFind(self Gram, match func() bool) Gram {
	return nil
}

func GramFindCode(self Gram, code GramCode) Gram {
	return nil
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

func (self *Parser) Parse() *Parser {
	// lex
	self.lexer = NewLexer(self.source)
	self.tokens = self.lexer.ParseAllTokens()

	// turn tokens into grams
	self.grams = tokensToGrams(self.tokens)

	// create module
	module := NewGramModule()
	self.root = module

	// parse at module
	module.Parse(self.grams)

	return self
}

func (self *Parser) Transform(root Gram) {}

func NewParser(source string) *Parser {
	parser := new(Parser)
	*parser = Parser {
		source: source,
	}
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

func NewGramModule() *GramModule {
	gram := new(GramModule)
	*gram = GramModule {
		GramBase: GramBase {
			code: GRAM_MODULE,
		},
	}
	return gram
}

func (self GramModule) Parse(grams *list.List) {}

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
	parse(*list.List, *list.Element) (*list.Element, bool)
}

func _parseGrams(
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
				matchfn, ok :=
					target[i].(func(*list.List, *list.Element) (*list.Element, bool));
				ok {
				if nextE, match := matchfn(grams, e); match {
					i++
					e = nextE
				} else {
					break
				}
			} else if
				matchfn, ok :=
					target[i].(func(interface {}, *list.Element) (*list.Element, bool));
				ok {
				if nextE, match := matchfn(grams, e); match {
					i++
					e = nextE
				} else {
					break
				}
			} else if matcher, ok := target[i].(Matcher); ok {
				if nextE, ok := matcher.parse(grams, e); ok {
					i++
					e = nextE
				} else {
					break
				}
			} else if capture, ok := target[i].(Capture); ok {
				if last, match := parseGrams(grams, e, capture.target); match {
					i++
					e = captureGrams(grams, e, last, capture.result).Next()
				} else {
					break
				}
			} else if match, ok := target[i].(MatchThen); ok {
				fail := true
				if _, ok := parseGrams(grams, e, match.target); ok {
					if nextE, ok := parseGrams(grams, e, match.then); ok {
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
				if nextE, ok := parseGrams(grams, e, match.target); ok {
					e = nextE
				}
			} else if match, ok := target[i].(MatchAnyTimes); ok {
				i++
				nextE, ok := e, true
				for ok {
					if nextE, ok = parseGrams(grams, nextE, match.target); ok {
						e = nextE
					}
				}
			} else if match, ok := target[i].(MatchAny); ok {
				nextE, ok := e, false
				for j := 0; j < len(match.target) && !ok; j++ {
					nextE, ok = parseGrams(grams, e, match.target[j])
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
	grams interface {}, gramE *list.Element, target interface{},
) (*list.Element, bool) {
	if gramsString, ok := grams.(string); ok {
		tokens := parseToGrams(gramsString)
		return parseGrams(tokens, tokens.Front(), target)
	}

	if gramsList, ok := grams.(*list.List); ok {
		if targetArray, ok := target.([]interface {}); ok {
			return _parseGrams(gramsList, gramE, targetArray)
		} else {
			return _parseGrams(gramsList, gramE, []interface {} { target })
		}
	}

	return nil, false
}

func (self Match) parse(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, self.target)
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

func CaptureExpr(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, Capture { MatchAny { []interface {} {
		CaptureLiteral,
	}}, GRAM_EXPR })
}

func MatchLines(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, MatchAnyTimes { MatchAny { []interface {} {
		GRAM_TOKEN_WS,
		GRAM_TOKEN_NEWLINE,
	}}})
}

func MatchWS(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, MatchAnyTimes {
		GRAM_TOKEN_WS,
	})
}

func RequireLines(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, []interface {} {
		GRAM_TOKEN_NEWLINE,
		MatchLines,
	})
}

func CaptureLiteralMember(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, MatchAnyTimes { MatchAny { []interface {} {
		Capture { MatchAny { []interface {} {
			[]interface {} {
				Capture { []interface {} {
					GRAM_TOKEN_NAME,
					GRAM_TOKEN_COLON,
					MatchWS,
					CaptureExpr,
					GRAM_TOKEN_COMMA,
					MatchLines,
				}, GRAM_LITERAL_NAMED_MEMBER },
				CaptureLiteralMember,
			},
			Capture { []interface {} {
				GRAM_TOKEN_NAME,
				GRAM_TOKEN_COLON,
				MatchWS,
				MatchAny { []interface {} { CaptureExpr, GRAM_EXPR }},
				MatchWS,
			}, GRAM_LITERAL_NAMED_MEMBER },
		}}, GRAM_LITERAL_NAMED_MEMBER_NODE },
		Capture { MatchAny { []interface {} {
			[]interface {} {
				Capture { []interface {} {
					CaptureExpr,
					GRAM_TOKEN_COMMA,
					MatchLines,
				}, GRAM_LITERAL_MEMBER },
				CaptureLiteralMember,
			},
			Capture { []interface {} {
				MatchAny { []interface {} { CaptureExpr, GRAM_EXPR }},
				MatchWS,
			}, GRAM_LITERAL_MEMBER },
		}}, GRAM_LITERAL_MEMBER_NODE },
	}}})
}

func MatchFuncTypeArg(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, MatchAny { []interface {} {
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
	}})
}

func MatchFuncTypeSig(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, []interface {} {
		GRAM_TOKEN_PARAN_BEGIN,
		MatchLines,
		MatchFuncTypeArg,
		GRAM_TOKEN_PARAN_END,
		MatchWS,
		MatchMaybe { MatchType },
	})
}

func MatchFuncType(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, []interface {} {
		GRAM_TOKEN_FUNC,
		MatchFuncTypeSig,
	})
}

func MatchObjectMember(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, MatchAnyTimes { []interface {} {
		GRAM_TOKEN_NAME,
		MatchAnyTimes { []interface {} {
			GRAM_TOKEN_COMMA, MatchWS, GRAM_TOKEN_NAME,
		}},
		GRAM_TOKEN_WS,
		MatchType,
		RequireLines,
	}})
}

func MatchInterfaceMember(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, MatchAnyTimes { []interface {} {
		GRAM_TOKEN_NAME,
		MatchFuncTypeSig,
		RequireLines,
	}})
}

func MatchType(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, MatchAny { []interface {} {
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

func CaptureFuncTypeArg(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, MatchAny { []interface {} {
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
	}})
}

func CaptureFuncTypeSig(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, []interface {} {
		GRAM_TOKEN_PARAN_BEGIN,
		MatchLines,
		CaptureFuncTypeArg,
		GRAM_TOKEN_PARAN_END,
		MatchWS,
		MatchMaybe { CaptureType },
	})
}

func CaptureFuncType(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, Capture { []interface {} {
		GRAM_TOKEN_FUNC,
		CaptureFuncTypeSig,
	}, GRAM_FUNC })
}

func CaptureObjectMember(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, []interface {} {
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

func CaptureInterfaceMember(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, MatchAnyTimes { Capture { []interface {} {
		GRAM_TOKEN_NAME,
		CaptureFuncTypeSig,
		RequireLines,
	}, GRAM_INTERFACE_MEMBER }})
}

func CaptureType(grams *list.List, start *list.Element) (*list.Element, bool) {
	return parseGrams(grams, start, Capture { MatchAny { []interface {} {
		[]interface {} {
			GRAM_TOKEN_SQUARE_BEGIN,
			GRAM_TOKEN_SQUARE_END,
			CaptureType,
		},
		[]interface {} {
			GRAM_TOKEN_ASTERICK,
			CaptureType,
		},
		Capture { []interface {} {
			GRAM_TOKEN_OBJECT,
			MatchWS,
			GRAM_TOKEN_CURLY_BEGIN,
			MatchLines,
			CaptureObjectMember,
			GRAM_TOKEN_CURLY_END,
		}, GRAM_OBJECT_TYPE },
		Capture { []interface {} {
			GRAM_TOKEN_INTERFACE,
			MatchWS,
			GRAM_TOKEN_CURLY_BEGIN,
			MatchLines,
			CaptureInterfaceMember,
			GRAM_TOKEN_CURLY_END,
		}, GRAM_INTERFACE_TYPE },
		CaptureFuncType,
		GRAM_TOKEN_NAME,
	}}, GRAM_TYPE })
}

func CaptureLiteral(grams *list.List, start *list.Element) (
	*list.Element, bool,
) {
	return parseGrams(grams, start, Capture { MatchAny { []interface {} {
		GRAM_TOKEN_INT,
		GRAM_TOKEN_REAL,
		GRAM_TOKEN_CHAR,
		GRAM_TOKEN_STRING,
		MatchThen { []interface {} {
			MatchType, MatchWS, GRAM_TOKEN_CURLY_BEGIN,
		}, Capture { []interface {} {
			CaptureType, MatchWS, GRAM_TOKEN_CURLY_BEGIN,
			MatchLines, CaptureLiteralMember,
			GRAM_TOKEN_CURLY_END,
		}, GRAM_LITERAL_OBJECT }},
		Capture { []interface {} {
			GRAM_TOKEN_CURLY_BEGIN,
			 MatchLines, CaptureLiteralMember,
			GRAM_TOKEN_CURLY_END,
		}, GRAM_LITERAL_OBJECT },
	}}, GRAM_LITERAL })
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
