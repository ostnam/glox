package ast

import (
	"github.com/ostnam/glox/pkg/tokens"
)

type Ast interface {}

type AstVisitor[T any] interface {
    VisitBinop(binop Binop) T
    VisitUnop(unop Unop) T
    VisitInt(val Int) T
    VisitStr(val Str) T
    VisitBool(val Bool) T
    VisitNil(val Nil) T
    VisitGrouping(val Grouping) T
}

func VisitAny[T any](visitor AstVisitor[T], node Ast) T {
    switch node.(type) {
    case Binop:
        return visitor.VisitBinop(node.(Binop))
    case Unop:
        return visitor.VisitUnop(node.(Unop))
    case Int:
        return visitor.VisitInt(node.(Int))
    case Str:
        return visitor.VisitStr(node.(Str))
    case Bool:
        return visitor.VisitBool(node.(Bool))
    case Nil:
        return visitor.VisitNil(node.(Nil))
    case Grouping:
        return visitor.VisitGrouping(node.(Grouping))
    default:
        panic(10)
    }
}

type Unop struct {
    Op UnaryOperator
    Val Ast
}
type UnaryOperator uint8
const (
    Not UnaryOperator = iota
    Neg
)

type Binop struct {
    Op BinaryOperator
    Lhs Ast
    Rhs Ast
}
type BinaryOperator uint8
const (
    Eql BinaryOperator = iota
    NotEql
    Minus
    Plus
    Mult
    Div
    Greater
    GreaterEql
    Less
    LessEql
)

type Int struct {
    Val int64
}

type Str struct {
    Val string 
}

type Bool struct {
    Val bool 
}

type Nil struct {
}

type Grouping struct {
    Expr Ast
}

type PrettyPrinter struct {}

var TokToBinop = map[tokens.TokType]BinaryOperator {
    tokens.EqlEql: Eql, 
    tokens.BangEql: NotEql,
    tokens.Minus: Minus,
    tokens.Plus: Plus,
    tokens.Star: Mult,
    tokens.Slash: Div,
    tokens.Greater: Greater,
    tokens.GreaterEql: GreaterEql,
    tokens.Less: Less,
    tokens.LessEql: LessEql,
}

var TokToUnop = map[tokens.TokType]UnaryOperator {
    tokens.Bang: Not, 
    tokens.Minus: Neg,
}
