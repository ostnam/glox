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

// Ast node for binary operations
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

// AST node for numeric value
type Num struct {
	Val float64
}

// AST node for strings
type Str struct {
	Val string
}

// AST node for bools
type Bool struct {
	Val bool
}

// AST node for the lox nil
type Nil struct {
}

// AST node for expressions between parens
type Grouping struct {
	Expr Ast
}

// AST node for statements
type Stmt struct {
	Kind StmtKind
	Expr Ast
}

type StmtKind int8

const (
	ExprStmt StmtKind = iota
	PrintStmt
)

type Identifier struct {
	Name string
}

type VarDecl struct {
	Name Identifier
}

type VarInit struct {
	Name Identifier
	Val  Ast
}

type Assignment struct {
	Name Identifier
	Val  Ast
}

type Block struct {
	Statements []Ast
}

type IfStmt struct {
    Pred Ast
    Body Ast
    Else Ast
}

type Or struct {
    Lhs Ast
    Rhs Ast
}

type And struct {
    Lhs Ast
    Rhs Ast
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
	case Stmt:
		node := node.(Stmt)
		fmt.Printf("Stmt: %v, Expr: ", node.Kind)
		prettyPrintAst(node.Expr, indent)
	case VarDecl:
		node := node.(VarDecl)
		fmt.Printf("Variable declaration: %v", node.Name.Name)
	case VarInit:
		node := node.(VarInit)
		fmt.Printf("Initializing %v to the value of:\n", node.Name)
		prettyPrintAst(node.Val, indent+INDENT_LVL)
	case Binop:
		node := node.(Binop)
		fmt.Printf("Binop: %s\n", node.Op)
		prettyPrintAst(node.Lhs, indent+INDENT_LVL)
		prettyPrintAst(node.Rhs, indent+INDENT_LVL)
	case Unop:
		node := node.(Unop)
		fmt.Printf("Unop: %s\n", node.Op)
		prettyPrintAst(node.Val, indent+INDENT_LVL)
	case Num:
		node := node.(Num)
		fmt.Printf("Num %f", node.Val)
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
	case Identifier:
		node := node.(Identifier)
		fmt.Printf("Identifier: %s", node.Name)
	case Assignment:
		node := node.(Assignment)
		fmt.Printf("Assigning to the name %s the value:\n", node.Name.Name)
		prettyPrintAst(node.Val, indent+INDENT_LVL)
	case Block:
		node := node.(Block)
		fmt.Printf("Block, with expressions:\n")
		for _, stmt := range node.Statements {
			prettyPrintAst(stmt, indent+INDENT_LVL)
		}
    case IfStmt:
        node := node.(IfStmt)
        fmt.Printf("If statement, with predicate:\n")
        prettyPrintAst(node.Pred, indent+INDENT_LVL)
        fmt.Printf("Body:\n")
        prettyPrintAst(node.Body, indent+INDENT_LVL)
        if node.Else != nil {
            fmt.Printf("Else case:\n")
            prettyPrintAst(node.Else, indent+INDENT_LVL)
        }

    case Or:
        node := node.(Or)
        fmt.Printf("OR:\n")
        prettyPrintAst(node.Lhs, indent+INDENT_LVL)
        prettyPrintAst(node.Rhs, indent+INDENT_LVL)

    case And:
        node := node.(And)
        fmt.Printf("OR:\n")
        prettyPrintAst(node.Lhs, indent+INDENT_LVL)
        prettyPrintAst(node.Rhs, indent+INDENT_LVL)

	default:
		fmt.Printf("Error pretty-printing AST, unknown node type: %s", t)
	}
	fmt.Print("\n")
}
