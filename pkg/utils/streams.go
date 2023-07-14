package utils

import (
	"github.com/ostnam/glox/pkg/tokens"
)

// Gets the next token, or nil if the end of the slice is reached.
func Peek[T any](str []T, pos int) *T {
	if pos < len(str) {
		return &str[pos]
	}
	return nil
}

// Gets the previous token, or nil if the end of the slice is reached.
func Previous[T any](str []T, pos int) *T {
	prevIdx := pos - 1
	return Advance(str, &prevIdx)
}

// Returns a reference to the next token, and advances the position.
// Returns nil if the position is outside of the length of the slice.
func Advance[T any](str []T, pos *int) *T {
	if *pos >= len(str) || *pos < 0 {
		return nil
	}
	res := &str[*pos]
	*pos++
	return res
}

func IsAtEnd[T any](bytes []T, pos int) bool {
	return pos >= len(bytes)
}

// If the next value is in vals, advance and return true.
// Otherwise return false, with the position unchanged.
func Match[T comparable](slice []T, pos *int, vals ...T) bool {
	next := Peek(slice, *pos)
	if next == nil {
		return false
	}
	for _, val := range vals {
		if *next == val {
			Advance(slice, pos)
			return true
		}
	}
	return false
}

// Match for a slice of tokens, with expected values being TokType.
func MatchTokenType(slice []tokens.Token, pos *int, vals ...tokens.TokType) bool {
	newVals := []tokens.TokType{}
	for _, val := range slice {
		newVals = append(newVals, val.Type)
	}
	return Match(newVals, pos, vals...)

}

func PeekMatchesTokType(slice []tokens.Token, pos int, vals ...tokens.TokType) bool {
	peeked := Peek(slice, pos)
	if peeked == nil {
		return false
	}
	for _, val := range vals {
		if val == peeked.Type {
			return true
		}
	}
	return false
}
