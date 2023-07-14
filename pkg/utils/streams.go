package utils

import (
    "github.com/ostnam/glox/pkg/tokens"
)

func Peek[T any](str []T, pos int) *T {
    if pos < len(str) {
        return &str[pos]
    }
    return nil
}

func Previous[T any](str []T, pos int) *T {
    prevIdx := pos - 1
    return Advance(str, &prevIdx)
}

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

func Match[T comparable](slice []T, pos *int, vals ...T) bool {
    if *pos >= len(slice) {
        return false
    }
    for _, val := range vals {
        if slice[*pos] == val {
            *pos++
            return true
        }
    }
    return false
}

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
