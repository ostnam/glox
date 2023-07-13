package utils

func Peek[T any](str []T, pos int) *T {
    if pos < len(str) {
        return &str[pos]
    }
    return nil // '\0'
}
