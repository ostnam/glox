package eval

import (
    "fmt"
    "reflect"

    "github.com/ostnam/glox/pkg/ast"
    "github.com/ostnam/glox/pkg/tokens"
)

type evalState struct {}

type RunTimeError struct {
    Kind RunTimeErrorKind
    Token tokens.Token
    Msg string
}

func (self RunTimeError) Error() string {
    return self.Msg
}

type RunTimeErrorKind uint8

const (
    TypeError RunTimeErrorKind = iota
)

func Eval(node ast.Ast) (ast.Ast, error) {
	switch t := node.(type) {
	case ast.Binop:
		node := node.(ast.Binop)
        evald_lhs, err := Eval(node.Lhs)
        if err != nil {
            return nil, err
        }
        evald_rhs, err := Eval(node.Rhs)
        if err != nil {
            return nil, err
        }
        switch node.Op {
        case ast.Plus:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                err := checkStrOperands(evald_lhs, evald_rhs)
                if err != nil {
                    return nil, err
                }
                return ast.Str { Val: evald_lhs.(ast.Str).Val + evald_rhs.(ast.Str).Val }, nil
            }
            return ast.Num { Val: evald_lhs.(ast.Num).Val + evald_rhs.(ast.Num).Val }, nil
        case ast.Minus:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Num { Val: evald_lhs.(ast.Num).Val - evald_rhs.(ast.Num).Val }, nil
        case ast.Mult:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Num { Val: evald_lhs.(ast.Num).Val * evald_rhs.(ast.Num).Val }, nil
        case ast.Div:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Num { Val: evald_lhs.(ast.Num).Val / evald_rhs.(ast.Num).Val }, nil
        case ast.Greater:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Bool { Val: evald_lhs.(ast.Num).Val > evald_rhs.(ast.Num).Val }, nil
        case ast.GreaterEql:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Bool { Val: evald_lhs.(ast.Num).Val >= evald_rhs.(ast.Num).Val }, nil
        case ast.Less:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Bool { Val: evald_lhs.(ast.Num).Val < evald_rhs.(ast.Num).Val }, nil
        case ast.LessEql:
            err := checkNumOperands(evald_lhs, evald_rhs)
            if err != nil {
                return nil, err
            }
            return ast.Bool { Val: evald_lhs.(ast.Num).Val <= evald_rhs.(ast.Num).Val }, nil
        case ast.Eql:
            val, err := isEql(evald_lhs, evald_rhs)
            return ast.Bool{Val: val}, err
        case ast.NotEql:
            val, err := isEql(evald_lhs, evald_rhs)
            return ast.Bool{Val: !val}, err

        default:
            return nil, fmt.Errorf("Unimplemented binary operator: %s", node.Op)
        }

	case ast.Unop:
		node := node.(ast.Unop)
        evald, err := Eval(node.Val)
        if err != nil {
            return nil, err
        }
        switch node.Op {
        case ast.Neg:
            switch evald.(type) {
            case ast.Num:
                evald := evald.(ast.Num)
                return ast.Num {Val: -evald.Val}, nil
            default: 
            msg := fmt.Sprintf("can't negate non-number: %#v", evald)
            return nil, RunTimeError{ Kind: TypeError, Msg: msg }
            }
        case ast.Not:
            return isTruthy(evald), nil
        default:
            return nil, fmt.Errorf("BUG: Unhandled unary operator in eval: %s", node.Op)
        }

	case ast.Num, ast.Str, ast.Bool, ast.Nil:
        return node, nil

	case ast.Grouping:
		node := node.(ast.Grouping)
        return Eval(node.Expr)

	default:
        return nil, fmt.Errorf("BUG: unmatched AST node type during evaluation: %T", t)
	}
}

func isTruthy(val ast.Ast) bool {
    switch val.(type) {
        case ast.Nil:
            return false
        case ast.Bool:
            val := val.(ast.Bool)
            return val.Val
        default:
            return true
    }
}

func isEql(lhs ast.Ast, rhs ast.Ast) (bool, error) {
    l := reflect.TypeOf(lhs)
    r := reflect.TypeOf(rhs)
    if l != r {
        return false, fmt.Errorf("Can't compare equality of values with two different types: %#v and %#v", lhs, rhs)

    }
    switch lhs.(type) {
        case ast.Num:
            lhs := lhs.(ast.Num)
            rhs := rhs.(ast.Num)
            return lhs.Val == rhs.Val, nil
        case ast.Str:
            lhs := lhs.(ast.Str)
            rhs := rhs.(ast.Str)
            return lhs.Val == rhs.Val, nil
        case ast.Bool:
            lhs := lhs.(ast.Bool)
            rhs := rhs.(ast.Bool)
            return lhs.Val == rhs.Val, nil
        default:
            return false, fmt.Errorf("Can't compare the value of non-primitive types: %#v and %#v", l, r)
    }
}

func checkNumOperands(lhs ast.Ast, rhs ast.Ast) error {
    _, ok := lhs.(ast.Num)
    if !ok {
        return RunTimeError {
            Kind: TypeError,
            Msg: fmt.Sprintf("Expected number but got: %#v", lhs),
        }
    }
    _, ok = rhs.(ast.Num)
    if !ok {
        return RunTimeError {
            Kind: TypeError,
            Msg: fmt.Sprintf("Expected number but got: %#v", rhs),
        }
    }
    return nil
}

func checkStrOperands(lhs ast.Ast, rhs ast.Ast) error {
    _, ok := lhs.(ast.Str)
    if !ok {
        return RunTimeError {
            Kind: TypeError,
            Msg: fmt.Sprintf("Expected str but got: %#v", lhs),
        }
    }
    _, ok = rhs.(ast.Str)
    if !ok {
        return RunTimeError {
            Kind: TypeError,
            Msg: fmt.Sprintf("Expected str but got: %#v", rhs),
        }
    }
    return nil
}

