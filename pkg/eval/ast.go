// Defines the types used to represent Lox programs (as AST nodes)
package eval

import (
	"fmt"
	"strings"

	"github.com/ostnam/glox/pkg/tokens"
)

// Interface of every Ast node type
type Ast interface{}

// Interface of every Ast callable type
type Callable interface {
	Call([]Ast, Interpreter) (Ast, error)
	Arity() int // number of args a function takes
}

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

// Used when pretty-printing the AST
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

// Used when pretty-printing the AST
func (self BinaryOperator) String() string {
	return []string{"Eql", "NotEql", "Minus", "Plus", "Mult", "Div", "Greater", "GreaterEql", "Less", "LessEql"}[self]
}

// AST node for numeric values
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
type Nil struct{}

// AST node for expressions between parentheses
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

// The name of a variable
type Identifier struct {
	Name string

	// Different variables sharing the same name can be declared in different scopes.
	//
	// An identifier refers to the latest declared variable with that name, in the
	// innermost scope that encloses the current expression.
	//
	// Before executing any Lox code, the resolver will statically analyze it to
	// set the Depth field, which indicates how many scopes do we have to look back
	// through to reach the variable that an identifier refers to.
	Depth int
}

// AST node for declaring a variable without setting a value, ie:
// var x;
type VarDecl struct {
	Name Identifier // Name of the variable being declared
}

// AST node for declaring a variable and setting a value, ie:
// var x = 10;
type VarInit struct {
	Name Identifier
	Val  Ast
}

// AST node for setting a new value to a variable, ie:
// x = 11;
type Assignment struct {
	Name Identifier
	Val  Ast
}

// AST node for blocks
type Block struct {
	Statements []Ast
}

// AST node for if statements
type IfStmt struct {
	Pred Ast
	Body Ast
	Else Ast
}

// AST node for boolean or expressions
type Or struct {
	Lhs Ast
	Rhs Ast
}

// AST node for boolean and expressions
type And struct {
	Lhs Ast
	Rhs Ast
}

// AST node for while loops
// This is the only looping construct in Lox
// (for loops are desugared into equivalent while loops)
type While struct {
	Pred Ast
	Body Ast
}

// AST node for function calls
type FnCall struct {
	Fn   Ast
	Args []Ast
}

// AST node for primitive functions
type BuiltinFn struct {
	CallFn   func([]Ast) (Ast, error)
	ArityVal int
}

func (fn BuiltinFn) Call(args []Ast, interpreter Interpreter) (Ast, error) {
	return fn.CallFn(args)
}

func (fn BuiltinFn) Arity() int {
	return fn.ArityVal
}

// AST node for user-defined functions
type Fn struct {
	Name    string   // name of the function
	Params  []string // name of the arguments it takes
	Body    Ast
	Closure *Env // pointer to the environment in which the function was created
	Type    FnType
	IsCtor  bool
}

type FnType uint8

const (
	None FnType = iota
	Regular
	Method
	Ctor
)

func (fn Fn) Call(args []Ast, interpreter Interpreter) (Ast, error) {
	newInterpreter := interpreter.newFnScope(fn.Params, args, fn.Closure)
	val, err := newInterpreter.Eval(fn.Body)
	if err != nil {
		return nil, err
	}

	// We need to unwrap return statements, since evaluating their value
	// stops the execution of the current block.
	ret, isRet := val.(ReturnStmt)
	if isRet {
		return ret.Val, nil
	}

	// Constructors should return `this`.
	if fn.IsCtor {
		return fn.Closure.getThis(This{Depth: 0})
	}

	// A function without return statement evaluates to nil.
	return Nil{}, nil
}

func (fn Fn) Arity() int {
	return len(fn.Params)
}

// Creates a new environment where the 'this' value is set to the argument
// passed to this function.
func (fn Fn) bind(this *ClassInstance) Fn {
	newEnv := fn.Closure.newChildren()
	newEnv.Store["this"] = this
	fn.Closure = &newEnv
	return fn
}

// AST node for functions declaration (ie: fun x() { return 10; })
type FnDecl struct {
	Fn
}

func (fn FnDecl) AsFn(env *Env) Fn {
	return Fn{
		Name:    fn.Name,
		Params:  fn.Params,
		Body:    fn.Body,
		Closure: env,
	}
}

type ReturnStmt struct {
	Val Ast
}

// AST node for class-declaration statements.
type ClassDecl struct {
	Name    string
	Methods []Fn
}

// AST node used to represent classes at runtime.
type Class struct {
	Name    string
	Methods map[string]Fn
}

// Calling a class == instantiating it.
func (cls Class) Call(args []Ast, inter Interpreter) (Ast, error) {
	inst := ClassInstance{Class: &cls, Fields: map[string]Ast{}}
	ctor, ok := cls.Methods["init"]
	if ok {
		ctor.bind(&inst).Call(args, inter)
	}
	return inst, nil
}

func (cls Class) Arity() int {
	ctor, ok := cls.Methods["init"]
	if !ok {
		return 0
	}
	return ctor.Arity()
}

type ClassInstance struct {
	Class  *Class
	Fields map[string]Ast
}

// Called to retrieve a field/method of a class instance.
func (instance ClassInstance) get(fieldName string) (Ast, error) {
	val, ok := instance.Fields[fieldName]
	if ok {
		return val, nil
	}
	meth, ok := instance.Class.Methods[fieldName]
	if ok {
		return meth.bind(&instance), nil
	}
	return nil, fmt.Errorf("Class instance has no field named %s", fieldName)
}

// Called to set a field of a class instance.
func (instance ClassInstance) set(fieldName string, val Ast) {
	instance.Fields[fieldName] = val
}

// AST node for expressions accessing a field of a class instance.
type Get struct {
	Lhs       Ast
	FieldName string
}

// AST node for expressions setting a field of a class instance.
type Set struct {
	Lhs       Ast
	FieldName string
	Val       Ast
}

// AST node for the this keyword.
type This struct {
	Depth int // set by the resolver, in the same manner as for Identifiers
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
	case While:
		node := node.(While)
		fmt.Printf("WHILE:\n")
		prettyPrintAst(node.Pred, indent+INDENT_LVL)
		fmt.Printf("DO:\n")
		prettyPrintAst(node.Body, indent+INDENT_LVL)
	case FnCall:
		node := node.(FnCall)
		fmt.Printf("Function call: with function being the value of:")
		prettyPrintAst(node.Fn, indent+INDENT_LVL)
		fmt.Printf("And the args:")
		for _, arg := range node.Args {
			prettyPrintAst(arg, indent+INDENT_LVL)
		}
	case BuiltinFn:
		fmt.Printf("<builtin function>")
	case Fn:
		node := node.(Fn)
		fmt.Printf("Function named %s that takes parameters: ", node.Name)
		first := true
		for _, param := range node.Params {
			if !first {
				fmt.Printf(". ")
			}
			first = false
			fmt.Printf("%s", param)
		}
		fmt.Printf(" and with body:")
		prettyPrintAst(node.Body, indent+INDENT_LVL)
	case FnDecl:
		node := node.(FnDecl)
		fmt.Printf("Declaration of function named %s with parameters: ", node.Name)
		first := true
		for _, param := range node.Params {
			if !first {
				fmt.Printf(". ")
			}
			first = false
			fmt.Printf("%s", param)
		}
		fmt.Printf(" and with body:\n")
		prettyPrintAst(node.Body, indent+INDENT_LVL)
	case ReturnStmt:
		node := node.(ReturnStmt)
		fmt.Printf("Return:\n")
		prettyPrintAst(node.Val, indent+INDENT_LVL)
	case ClassDecl:
		node := node.(ClassDecl)
		fmt.Printf("Class declaration of %s with methods:\n", node.Name)
		for _, fn := range node.Methods {
			prettyPrintAst(fn, indent+INDENT_LVL)
		}
	case Class:
		node := node.(Class)
		fmt.Printf("Class %s", node.Name)
	case ClassInstance:
		node := node.(ClassInstance)
		fmt.Printf("Instance of class %s", node.Class.Name)
	default:
		fmt.Printf("Error pretty-printing AST, unknown node type: %s", t)
	}
	fmt.Print("\n")
}
