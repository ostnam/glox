package ast

import (
	"fmt"
	"strings"

	"github.com/ostnam/glox/pkg/tokens"
)

// Interface of every Ast node type
type Ast interface{}

// Ast node for unary operations
type Unop struct {
	Op  UnaryOperator
	Val Ast
}

type UnaryOperator uint8

const (
	Not UnaryOperator = iota
	Neg
)

func (self UnaryOperator) String() string {
	return []string{"Not", "Neg"}[self]
}

type Binop struct {
	Op  BinaryOperator
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

func (self BinaryOperator) String() string {
	return []string{"Eql", "NotEql", "Minus", "Plus", "Mult", "Div", "Greater", "GreaterEql", "Less", "LessEql"}[self]
}

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

// Maps tokens to their corresponding BinaryOperator if such a mapping exists.
var TokToBinop = map[tokens.TokType]BinaryOperator{
	tokens.EqlEql:     Eql,
	tokens.BangEql:    NotEql,
	tokens.Minus:      Minus,
	tokens.Plus:       Plus,
	tokens.Star:       Mult,
	tokens.Slash:      Div,
	tokens.Greater:    Greater,
	tokens.GreaterEql: GreaterEql,
	tokens.Less:       Less,
	tokens.LessEql:    LessEql,
}

// Maps tokens to their corresponding UnaryOperator if such a mapping exists.
var TokToUnop = map[tokens.TokType]UnaryOperator{
	tokens.Bang:  Not,
	tokens.Minus: Neg,
}

// Pretty prints an AST node
func PrettyPrintAst(node Ast) {
	prettyPrintAst(node, 0)
}

func prettyPrintAst(node Ast, indent int) {
	const INDENT_LVL = 3
	if indent == 0 {
		fmt.Print(strings.Repeat(" ", indent))
	} else {
		fmt.Print(strings.Repeat(" ", indent-1) + "|" + " ")
	}
	switch t := node.(type) {
	case Binop:
		node := node.(Binop)
		fmt.Printf("Binop: %s\n", node.Op)
		prettyPrintAst(node.Lhs, indent+INDENT_LVL)
		prettyPrintAst(node.Rhs, indent+INDENT_LVL)
	case Unop:
		node := node.(Unop)
		fmt.Printf("Unop: %s\n", node.Op)
		prettyPrintAst(node.Val, indent+INDENT_LVL)
	case Int:
		node := node.(Int)
		fmt.Printf("Int %d", node.Val)
	case Str:
		node := node.(Str)
		fmt.Printf("Str: %s", node.Val)
	case Bool:
		node := node.(Bool)
		fmt.Printf("Bool: %t", node.Val)
	case Nil:
		fmt.Printf("Nil")
	case Grouping:
		node := node.(Grouping)
		fmt.Printf("Grouping\n")
		prettyPrintAst(node.Expr, indent+INDENT_LVL)
	default:
		fmt.Printf("Error pretty-printing AST, unknown node type: %s", t)
	}
	fmt.Print("\n")
}
