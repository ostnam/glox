package tokens

type Token struct {
	Type    TokType
	Lexeme  string
	Literal string
	Line    int
}

type TokType int8

const (
	// single char tokens
	LeftParen TokType = iota
	RightParen
	LeftBrace
	RightBrace
	Comma
	Dot
	Minus
	Plus
	Semicolon
	Slash
	Star
	// 1 or 2 char tokens
	Bang
	BangEql
	Eql
	EqlEql
	Greater
	GreaterEql
	Less
	LessEql
	// literals
	Identifier
	Str
	Num
	// keywords
	And
	Class
	Else
	False
	Fun
	For
	If
	Nil
	Or
	Print
	Return
	Super
	This
	True
	Var
	While
	EOF
)
