package eval

import (
	"fmt"
	"reflect"
	"time"

	"github.com/ostnam/glox/pkg/tokens"
)

type Env struct {
	Parent *Env
	Store  map[string]Ast
}

type Interpreter struct {
	globals    *Env
	currentEnv *Env
}

func NewInterpreter() Interpreter {
	globals := newGlobals()
	return Interpreter{globals: &globals, currentEnv: &globals}
}

func newEnv() Env {
	return Env{
		Parent: nil,
		Store:  map[string]Ast{},
	}
}

func newGlobals() Env {
	store := map[string]Ast{}
	store["clock"] = BuiltinFn{
		CallFn: func(val []Ast) (Ast, error) {
			return Num{
				Val: float64(time.Now().UnixMilli()),
			}, nil
		},
		ArityVal: 0,
	}

	return Env{
		Parent: nil,
		Store:  store,
	}
}

func (env *Env) newChildren() Env {
	return Env{
		Parent: env,
		Store:  map[string]Ast{},
	}
}

func (env *Env) setVariable(name string, val Ast) {
	env.Store[name] = val
}

func (self Interpreter) newFnScope(params []string, args []Ast, closure *Env) Interpreter {
	env := newEnv()
	env.Parent = closure
	for i := 0; i < len(params); i++ {
		env.setVariable(params[i], args[i])
	}
	return Interpreter{
		globals:    self.globals,
		currentEnv: &env,
	}
}

func (self Interpreter) readVariable(id Identifier) (Ast, error) {
	if id.Depth == -1 {
		val, ok := self.globals.Store[id.Name]
		if !ok {
			return nil, fmt.Errorf("Tried to read undefined global variable %s", id.Name)
		}
		return val, nil
	}
	return self.currentEnv.readVariable(id)
}

func (env Env) readVariable(name Identifier) (Ast, error) {
	ancestor, err := env.ancestor(name.Depth)
	if err != nil {
		return nil, err
	}
	val, ok := ancestor.Store[name.Name]
	if !ok {
		return nil, fmt.Errorf("Tried to read undefined variable named %s", name.Name)
	}
	return val, nil
}

func (env Env) getThis(this This) (Ast, error) {
	ancestor, err := env.ancestor(this.Depth)
	if err != nil {
		return nil, err
	}
	val, ok := ancestor.Store["this"]
	if !ok {
		return nil, fmt.Errorf("Tried to read unset this.")
	}
	return val, nil
}

// Only updates a pre-existing variable. Return whether it was successful,
// ie updating a non-existing variable returns false.
// Value must have already been evaluated.
func (interpreter *Interpreter) updateVariable(id Identifier, val Ast) error {
	if id.Depth == -1 {
		interpreter.globals.Store[id.Name] = val
	}
	env, err := interpreter.currentEnv.ancestor(id.Depth)
	if err != nil {
		return err
	}
	env.Store[id.Name] = val
	return nil
}

// Pointer won't be nil if err isn't nil.
func (env Env) ancestor(dist int) (*Env, error) {
	current := &env
	for i := 0; i < dist; i++ {
		if current.Parent == nil {
			return nil, fmt.Errorf("INTERNAL BUG: Error getting ancestor of environment, requested ancestor at depth %d but no parent after ancestor at depth %d", dist, i)
		}
		current = current.Parent
	}
	return current, nil
}

func (self *Interpreter) Eval(node Ast) (Ast, error) {
	switch t := node.(type) {
	case This:
		node := node.(This)
		return self.currentEnv.getThis(node)

	case Set:
		node := node.(Set)
		evald_lhs, err := self.Eval(node.Lhs)
		if err != nil {
			return nil, err
		}
		lhs_inst, ok := evald_lhs.(ClassInstance)
		if !ok {
			return nil, fmt.Errorf("Can't set field of non class instance")
		}
		evald, err := self.Eval(node.Val)
		if err != nil {
			return nil, err
		}
		lhs_inst.set(node.FieldName, evald)
		return evald, nil

	case Get:
		node := node.(Get)
		evald, err := self.Eval(node.Lhs)
		if err != nil {
			return nil, err
		}
		instance, ok := evald.(ClassInstance)
		if !ok {
			return nil, fmt.Errorf("Tried to access field of non-class instance.")
		}
		return instance.get(node.FieldName)

	case ClassDecl:
		node := node.(ClassDecl)
		methods := map[string]Fn{}
		for _, method := range node.Methods {
			method.Closure = self.currentEnv
			if method.Name == "init" {
				method.IsCtor = true
			}
			methods[method.Name] = method
		}
		newClass := Class{
			Name:    node.Name,
			Methods: methods,
		}
		self.currentEnv.setVariable(node.Name, newClass)
		return Nil{}, nil

	case ReturnStmt:
		node := node.(ReturnStmt)
		evald, err := self.Eval(node.Val)
		if err != nil {
			return nil, err
		}
		return ReturnStmt{Val: evald}, nil

	case FnCall:
		node := node.(FnCall)
		fn, err := self.Eval(node.Fn)
		if err != nil {
			return nil, err
		}
		callable, ok := fn.(Callable)
		if !ok {
			return nil, fmt.Errorf("Called function on non-callable AST node: %v", fn)
		}
		if len(node.Args) != callable.Arity() {
			return nil, fmt.Errorf("Function %v called with incorrect number of arguments, expected: %d, got %d", callable, len(node.Args), callable.Arity())
		}
		args := []Ast{}
		for _, expr := range node.Args {
			evald, err := self.Eval(expr)
			if err != nil {
				return nil, err
			}
			args = append(args, evald)
		}
		return callable.Call(args, *self)

	case Fn:
		return node, nil

	case FnDecl:
		node := node.(FnDecl)
		self.currentEnv.setVariable(node.Name, node.AsFn(self.currentEnv))
		return Nil{}, nil

	case IfStmt:
		node := node.(IfStmt)
		evaldPred, err := self.Eval(node.Pred)
		if err != nil {
			return nil, err
		}
		if isTruthy(evaldPred) {
			val, err := self.Eval(node.Body)
			if err != nil {
				return nil, err
			}
			ret, isRet := val.(ReturnStmt)
			if isRet {
				return ret, nil
			}
		} else if node.Else != nil {
			val, err := self.Eval(node.Else)
			if err != nil {
				return nil, err
			}
			ret, isRet := val.(ReturnStmt)
			if isRet {
				return ret, nil
			}
		}
		return Nil{}, nil

	case Block:
		node := node.(Block)
		old_env := self.currentEnv
		children_env := self.currentEnv.newChildren()
		self.currentEnv = &children_env
		for _, stmt := range node.Statements {
			val, err := self.Eval(stmt)
			if err != nil {
				return nil, err
			}
			ret, isRet := val.(ReturnStmt)
			if isRet {
				return ret, nil
			}
		}
		self.currentEnv = old_env
		return Nil{}, nil

	case VarDecl:
		node := node.(VarDecl)
		self.currentEnv.setVariable(node.Name.Name, Nil{})
		return Nil{}, nil

	case VarInit:
		node := node.(VarInit)
		evald, err := self.Eval(node.Val)
		if err != nil {
			return nil, err
		}
		self.currentEnv.setVariable(node.Name.Name, evald)
		return Nil{}, nil

	case Assignment:
		node := node.(Assignment)
		evald, err := self.Eval(node.Val)
		if err != nil {
			return nil, err
		}
		err = self.updateVariable(node.Name, evald)
		if err != nil {
			return nil, err
		}
		return evald, nil

	case Identifier:
		node := node.(Identifier)
		val, err := self.readVariable(node)
		if err != nil {
			return nil, err
		}
		return val, nil

	case Stmt:
		node := node.(Stmt)
		switch node.Kind {
		case ExprStmt:
			_, err := self.Eval(node.Expr)
			return Nil{}, err
		case PrintStmt:
			evald, err := self.Eval(node.Expr)
			if err != nil {
				return nil, err
			}
			fmt.Print(evald)
			return Nil{}, nil
		default:
			return nil, fmt.Errorf("Unhandled statement kind: %v", node.Kind)
		}

	case Binop:
		node := node.(Binop)
		evald_lhs, err := self.Eval(node.Lhs)
		if err != nil {
			return nil, err
		}
		evald_rhs, err := self.Eval(node.Rhs)
		if err != nil {
			return nil, err
		}
		switch node.Op {
		case Plus:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				err := checkStrOperands(evald_lhs, evald_rhs)
				if err != nil {
					return nil, err
				}
				return Str{Val: evald_lhs.(Str).Val + evald_rhs.(Str).Val}, nil
			}
			return Num{Val: evald_lhs.(Num).Val + evald_rhs.(Num).Val}, nil
		case Minus:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Num{Val: evald_lhs.(Num).Val - evald_rhs.(Num).Val}, nil
		case Mult:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Num{Val: evald_lhs.(Num).Val * evald_rhs.(Num).Val}, nil
		case Div:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Num{Val: evald_lhs.(Num).Val / evald_rhs.(Num).Val}, nil
		case Greater:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Bool{Val: evald_lhs.(Num).Val > evald_rhs.(Num).Val}, nil
		case GreaterEql:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Bool{Val: evald_lhs.(Num).Val >= evald_rhs.(Num).Val}, nil
		case Less:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Bool{Val: evald_lhs.(Num).Val < evald_rhs.(Num).Val}, nil
		case LessEql:
			err := checkNumOperands(evald_lhs, evald_rhs)
			if err != nil {
				return nil, err
			}
			return Bool{Val: evald_lhs.(Num).Val <= evald_rhs.(Num).Val}, nil
		case Eql:
			val, err := isEql(evald_lhs, evald_rhs)
			return Bool{Val: val}, err
		case NotEql:
			val, err := isEql(evald_lhs, evald_rhs)
			return Bool{Val: !val}, err

		default:
			return nil, fmt.Errorf("Unimplemented binary operator: %s", node.Op)
		}

	case Or:
		node := node.(Or)
		lhs, err := self.Eval(node.Lhs)
		if err != nil {
			return nil, err
		}
		if isTruthy(lhs) {
			return lhs, nil
		}
		return self.Eval(node.Rhs)

	case And:
		node := node.(And)
		lhs, err := self.Eval(node.Lhs)
		if err != nil {
			return nil, err
		}
		if !isTruthy(lhs) {
			return lhs, nil
		}
		return self.Eval(node.Rhs)

	case While:
		node := node.(While)
		for {
			val, err := self.Eval(node.Pred)
			if err != nil {
				return nil, err
			}
			if !isTruthy(val) {
				break
			}
			val, err = self.Eval(node.Body)
			if err != nil {
				return nil, err
			}
			ret, isRet := val.(ReturnStmt)
			if isRet {
				return ret, nil
			}
		}
		return Nil{}, nil

	case Unop:
		node := node.(Unop)
		evald, err := self.Eval(node.Val)
		if err != nil {
			return nil, err
		}
		switch node.Op {
		case Neg:
			switch evald.(type) {
			case Num:
				evald := evald.(Num)
				return Num{Val: -evald.Val}, nil
			default:
				msg := fmt.Sprintf("can't negate non-number: %#v", evald)
				return nil, RunTimeError{Kind: TypeError, Msg: msg}
			}
		case Not:
			return isTruthy(evald), nil
		default:
			return nil, fmt.Errorf("BUG: Unhandled unary operator in eval: %s", node.Op)
		}

	case Num, Str, Bool, Nil:
		return node, nil

	case Grouping:
		node := node.(Grouping)
		return self.Eval(node.Expr)

	default:
		return nil, fmt.Errorf("BUG: unmatched AST node type during evaluation: %T", t)
	}
}

func (self *Interpreter) resolve(node Ast, depth int) {
}

func isTruthy(val Ast) bool {
	switch val.(type) {
	case Nil:
		return false
	case Bool:
		val := val.(Bool)
		return val.Val
	default:
		return true
	}
}

func isEql(lhs Ast, rhs Ast) (bool, error) {
	l := reflect.TypeOf(lhs)
	r := reflect.TypeOf(rhs)
	if l != r {
		return false, fmt.Errorf("Can't compare equality of values with two different types: %#v and %#v", lhs, rhs)

	}
	switch lhs.(type) {
	case Num:
		lhs := lhs.(Num)
		rhs := rhs.(Num)
		return lhs.Val == rhs.Val, nil
	case Str:
		lhs := lhs.(Str)
		rhs := rhs.(Str)
		return lhs.Val == rhs.Val, nil
	case Bool:
		lhs := lhs.(Bool)
		rhs := rhs.(Bool)
		return lhs.Val == rhs.Val, nil
	default:
		return false, fmt.Errorf("Can't compare the value of non-primitive types: %#v and %#v", l, r)
	}
}

type RunTimeError struct {
	Kind  RunTimeErrorKind
	Token tokens.Token
	Msg   string
}

func (self RunTimeError) Error() string {
	return self.Msg
}

type RunTimeErrorKind uint8

const (
	TypeError RunTimeErrorKind = iota
)

func checkNumOperands(lhs Ast, rhs Ast) error {
	_, ok := lhs.(Num)
	if !ok {
		return RunTimeError{
			Kind: TypeError,
			Msg:  fmt.Sprintf("Expected number but got: %#v", lhs),
		}
	}
	_, ok = rhs.(Num)
	if !ok {
		return RunTimeError{
			Kind: TypeError,
			Msg:  fmt.Sprintf("Expected number but got: %#v", rhs),
		}
	}
	return nil
}

func checkStrOperands(lhs Ast, rhs Ast) error {
	_, ok := lhs.(Str)
	if !ok {
		return RunTimeError{
			Kind: TypeError,
			Msg:  fmt.Sprintf("Expected str but got: %#v", lhs),
		}
	}
	_, ok = rhs.(Str)
	if !ok {
		return RunTimeError{
			Kind: TypeError,
			Msg:  fmt.Sprintf("Expected str but got: %#v", rhs),
		}
	}
	return nil
}
