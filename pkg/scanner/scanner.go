package scanner

import (
	"fmt"
	"unicode"

	. "github.com/ostnam/glox/pkg/tokens"
	"github.com/ostnam/glox/pkg/utils"
)

// Top level scanning function
func Scan(input []rune) ([]Token, []error) {
	pos := 0
	lineNumber := 0
	toks := []Token{}
	errs := []error{}
	for !utils.IsAtEnd(input, pos) {
		tok, err := scanToken(input, &pos, &lineNumber)
		if err != nil {
			errs = append(errs, err)
		}
		if tok != nil {
			toks = append(toks, *tok)
		}
	}
	toks = append(
		toks,
		Token{
			Type:    EOF,
			Lexeme:  "",
			Literal: "",
			Line:    0,
		},
	)
	return toks, errs
}

// Attempts to scan a single token from the slice.
func scanToken(str []rune, pos *int, lineNumber *int) (*Token, error) {
	start := *pos
	c := utils.Advance(str, pos)
	if c == nil {
		return nil, nil
	}
	switch *c {
	// single-char tokens
	case '(':
		return mkToken(LeftParen, str, start, *pos, *lineNumber), nil
	case ')':
		return mkToken(RightParen, str, start, *pos, *lineNumber), nil
	case '{':
		return mkToken(LeftBrace, str, start, *pos, *lineNumber), nil
	case '}':
		return mkToken(RightBrace, str, start, *pos, *lineNumber), nil
	case ',':
		return mkToken(Comma, str, start, *pos, *lineNumber), nil
	case '.':
		return mkToken(Dot, str, start, *pos, *lineNumber), nil
	case '-':
		return mkToken(Minus, str, start, *pos, *lineNumber), nil
	case '+':
		return mkToken(Plus, str, start, *pos, *lineNumber), nil
	case ';':
		return mkToken(Semicolon, str, start, *pos, *lineNumber), nil
	case '*':
		return mkToken(Star, str, start, *pos, *lineNumber), nil
	// single or 2 chars tokens
	case '!':
		c := utils.Peek(str, *pos)
		if c == nil {
			return mkToken(Bang, str, start, *pos, *lineNumber), nil
		}
		switch *c {
		case '=':
			*pos += 1
			return mkToken(BangEql, str, start, *pos, *lineNumber), nil
		default:
			return mkToken(Bang, str, start, *pos, *lineNumber), nil
		}
	case '=':
		c := utils.Peek(str, *pos)
		if c == nil {
			return mkToken(Eql, str, start, *pos, *lineNumber), nil
		}
		switch *c {
		case '=':
			*pos += 1
			return mkToken(EqlEql, str, start, *pos, *lineNumber), nil
		default:
			return mkToken(Eql, str, start, *pos, *lineNumber), nil
		}
	case '>':
		c := utils.Peek(str, *pos)
		if c == nil {
			return mkToken(Greater, str, start, *pos, *lineNumber), nil
		}
		switch *c {
		case '=':
			*pos += 1
			return mkToken(GreaterEql, str, start, *pos, *lineNumber), nil
		default:
			return mkToken(Greater, str, start, *pos, *lineNumber), nil
		}
	case '<':
		c := utils.Peek(str, *pos)
		if c == nil {
			return mkToken(Less, str, start, *pos, *lineNumber), nil
		}
		switch *c {
		case '=':
			*pos += 1
			return mkToken(LessEql, str, start, *pos, *lineNumber), nil
		default:
			return mkToken(Less, str, start, *pos, *lineNumber), nil
		}
	case '/':
		c := utils.Peek(str, *pos)
		if c == nil {
			return mkToken(Slash, str, start, *pos, *lineNumber), nil
		}
		switch *c {
		case '/':
			consumeRestOfLine(str, pos)
			return nil, nil
		default:
			return mkToken(Slash, str, start, *pos, *lineNumber), nil
		}
	case '"':
		return scanStrLiteral(str, pos, start, lineNumber)
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return scanNumLiteral(str, pos, start, lineNumber)
	case ' ', '\t':
		return nil, nil
	case '\n':
		*lineNumber++
		return nil, nil
	default:
		if unicode.IsLetter(*c) {
			return scanIdentifier(str, pos, start, lineNumber)
		}
		err := fmt.Errorf("Line %d: incorrect token", *lineNumber)
		return nil, err
	}
}

// Attempts to scan a string literal, enclosed between double quotes.
// The first quote is assumed to already have been matched.
func scanStrLiteral(str []rune, pos *int, start int, line *int) (*Token, error) {
	initialLine := *line
	for {
		char := utils.Advance(str, pos)
		if char == nil {
			break
		}
		switch *char {
		case '"':
			return mkToken(Str, str, start+1, *pos-1, *line), nil
		case '\n':
			*line++
		}
	}
	err := fmt.Errorf("Unterminated string literal beginning at line %d", initialLine)
	return nil, err
}

func scanNumLiteral(str []rune, pos *int, start int, line *int) (*Token, error) {
loop:
	for !utils.IsAtEnd(str, *pos) {
		c := utils.Peek(str, *pos)
		if c == nil {
			break
		}
		switch *c {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			char := utils.Advance(str, pos)
			if char == nil {
				break loop
			}
		case '.':
			c := utils.Peek(str, *pos+1)
			if c == nil {
				return nil, fmt.Errorf("Error lexing number: . not followed by a number, line %d", line)
			}
			switch *c {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				char := utils.Advance(str, pos)
				if char == nil {
					break loop
				}
			}
		default:
			break loop
		}
	}
	return mkToken(Num, str, start, *pos, *line), nil
}

func scanIdentifier(str []rune, pos *int, start int, line *int) (*Token, error) {
    for {
        next := utils.Peek(str, *pos)
        if next == nil || !(unicode.IsLetter(*next) || unicode.IsDigit(*next)) {
            break
        }
        utils.Advance(str, pos)
    }
	identifier := string(str[start:*pos])
	tok, identifierIsKeyword := keywords[identifier]
	if identifierIsKeyword {
		return mkToken(tok, str, start, *pos, *line), nil
	} else {
		return mkToken(Identifier, str, start, *pos, *line), nil
	}
}

func mkToken(type_ TokType, str []rune, start int, pos int, line int) *Token {
    lexeme := string(str[start:pos])
	return &Token{
		Type:    type_,
		Lexeme:  lexeme,
		Literal: lexeme,
		Line:    line,
	}
}

func consumeRestOfLine(str []rune, pos *int) {
	for ; *pos < len(str); *pos++ {
		if str[*pos] == '\n' {
			*pos++
			return
		}
	}
}

var keywords = map[string]TokType{
	"and":    And,
	"class":  Class,
	"else":   Else,
	"false":  False,
	"for":    For,
	"fun":    Fun,
	"if":     If,
	"nil":    Nil,
	"or":     Or,
	"print":  Print,
	"return": Return,
	"super":  Super,
	"this":   This,
	"true":   True,
	"var":    Var,
	"while":  While,
}
