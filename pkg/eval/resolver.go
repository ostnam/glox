package eval

import "fmt"

type Resolver struct {
	scopes       []map[string]bool
	currentFn    FnType
	currentClass ClassType
}

type ClassType uint8

const (
	none ClassType = iota
	class
	subclass
)

func NewResolver() Resolver {
	return Resolver{
		scopes:       []map[string]bool{{}},
		currentFn:    None,
		currentClass: none,
	}
}

func (res Resolver) Resolve(node Ast) (Ast, error) {
	switch t := node.(type) {
	case Super:
		if res.currentClass != subclass {
			return nil, fmt.Errorf("Used 'super' outside of a subclass.")
		}
		node := node.(Super)
		super, err := res.Resolve(node.Super)
		if err != nil {
			return nil, err
		}
		node.Super = super.(Identifier)
		return node, nil

	case This:
		if res.currentClass == none {
			return nil, fmt.Errorf("Used 'this' outside of class declaration.")
		}
		node := node.(This)
		return res.resolveThis(node), nil

	case Set:
		node := node.(Set)
		newLhs, err := res.Resolve(node.Lhs)
		if err != nil {
			return nil, err
		}
		node.Lhs = newLhs
		newVal, err := res.Resolve(node.Val)
		if err != nil {
			return nil, err
		}
		node.Val = newVal
		return node, nil

	case Get:
		node := node.(Get)
		newLhs, err := res.Resolve(node.Lhs)
		if err != nil {
			return nil, err
		}
		node.Lhs = newLhs
		return node, nil

	case ClassDecl:
		node := node.(ClassDecl)
		prevClass := res.currentClass
		if node.Super != nil {
			res.currentClass = subclass
		} else {
			res.currentClass = class
		}
		res.declare(node.Name)
		res.define(node.Name)
		if node.Super != nil {
			if node.Super.Name == node.Name {
				return nil, fmt.Errorf("Class %s can't inherit from itself", node.Name)
			}
			res.beginScope()
			res.scopes[len(res.scopes)-1]["super"] = true
			resolvedSuper, err := res.Resolve(*node.Super)
			if err != nil {
				return nil, err
			}
			newSuper := resolvedSuper.(Identifier)
			node.Super = &newSuper
		}
		res.beginScope()
		res.scopes[len(res.scopes)-1]["this"] = true
		newMethods := []Fn{}
		for _, method := range node.Methods {
			if method.Name == "init" {
				method.Type = Ctor
			} else {
				method.Type = Method
			}
			method, err := res.Resolve(method)
			if err != nil {
				return nil, err
			}
			fn := method.(Fn)
			newMethods = append(newMethods, fn)
		}
		node.Methods = newMethods
		res.endScope()
		if node.Super != nil {
			res.endScope()
		}
		res.currentClass = prevClass

		return node, nil

	case VarInit:
		node := node.(VarInit)
		err := res.declare(node.Name.Name)
		if err != nil {
			return nil, err
		}
		newVal, err := res.Resolve(node.Val)
		if err != nil {
			return nil, err
		}
		res.define(node.Name.Name)
		return VarInit{node.Name, newVal}, nil

	case VarDecl:
		node := node.(VarDecl)
		err := res.declare(node.Name.Name)
		if err != nil {
			return nil, err
		}
		res.define(node.Name.Name)
		return node, nil

	case Identifier:
		node := node.(Identifier)
		l := len(res.scopes)
		alreadyDefined, alreadyDeclared := res.scopes[l-1][node.Name]
		if l != 0 && (alreadyDeclared && !alreadyDefined) {
			return nil, fmt.Errorf("Can't read variable %s in its own initializer", node.Name)
		}
		return res.resolveLocal(node), nil

	case Assignment:
		node := node.(Assignment)
		newVal, err := res.Resolve(node.Val)
		if err != nil {
			return nil, err
		}
		newName := res.resolveLocal(node.Name)
		return Assignment{Name: newName, Val: newVal}, nil

	case FnDecl:
		node := node.(FnDecl)
		err := res.declare(node.Name)
		if err != nil {
			return nil, err
		}
		res.define(node.Name)
		asFn, err := res.Resolve(node.AsFn(nil))
		return FnDecl{Fn: asFn.(Fn)}, err

	case Fn:
		node := node.(Fn)
		res.beginScope()
		for _, param := range node.Params {
			err := res.declare(param)
			if err != nil {
				return nil, err
			}
			res.define(param)
		}
		old_fn := res.currentFn
		res.currentFn = Regular
		newBody, err := res.Resolve(node.Body)
		node.Body = newBody
		res.currentFn = old_fn
		res.endScope()
		return node, err

	case Block:
		node := node.(Block)
		res.beginScope()
		newStmts := []Ast{}
		for _, stmt := range node.Statements {
			val, err := res.Resolve(stmt)
			if err != nil {
				return nil, err
			}
			newStmts = append(newStmts, val)
		}
		res.endScope()
		return Block{Statements: newStmts}, nil

	case Stmt:
		node := node.(Stmt)
		newExpr, err := res.Resolve(node.Expr)
		if err != nil {
			return nil, err
		}
		node.Expr = newExpr
		return node, nil

	case IfStmt:
		node := node.(IfStmt)
		newPred, err := res.Resolve(node.Pred)
		if err != nil {
			return nil, err
		}
		node.Pred = newPred
		newBody, err := res.Resolve(node.Body)
		if err != nil {
			return nil, err
		}
		node.Body = newBody
		if node.Else != nil {
			newElse, err := res.Resolve(node.Else)
			if err != nil {
				return nil, err
			}
			node.Else = newElse
		}
		return node, nil

	case ReturnStmt:
		node := node.(ReturnStmt)
		if res.currentFn == None {
			return nil, fmt.Errorf("Used return outside of a function body.")
		}
		if res.currentFn == Ctor {
			return nil, fmt.Errorf("Used return inside a constructor.")
		}
		newVal, err := res.Resolve(node.Val)
		node.Val = newVal
		return node, err

	case While:
		node := node.(While)
		new_pred, err := res.Resolve(node.Pred)
		if err != nil {
			return nil, err
		}
		node.Pred = new_pred
		newBody, err := res.Resolve(node.Body)
		if err != nil {
			return nil, err
		}
		node.Body = newBody
		return node, nil

	case Unop:
		node := node.(Unop)
		return res.Resolve(node.Val)

	case Binop:
		node := node.(Binop)
		newLhs, err := res.Resolve(node.Lhs)
		if err != nil {
			return nil, err
		}
		node.Lhs = newLhs
		newRhs, err := res.Resolve(node.Rhs)
		if err != nil {
			return nil, err
		}
		node.Rhs = newRhs
		return node, nil

	case And:
		node := node.(And)
		newLhs, err := res.Resolve(node.Lhs)
		if err != nil {
			return nil, err
		}
		node.Lhs = newLhs
		newRhs, err := res.Resolve(node.Rhs)
		if err != nil {
			return nil, err
		}
		node.Rhs = newRhs
		return node, nil

	case Or:
		node := node.(Or)
		newLhs, err := res.Resolve(node.Lhs)
		if err != nil {
			return nil, err
		}
		node.Lhs = newLhs
		newRhs, err := res.Resolve(node.Rhs)
		if err != nil {
			return nil, err
		}
		node.Rhs = newRhs
		return node, nil

	case FnCall:
		node := node.(FnCall)
		newFn, err := res.Resolve(node.Fn)
		if err != nil {
			return nil, err
		}
		node.Fn = newFn
		newArgs := []Ast{}
		for _, expr := range node.Args {
			newArg, err := res.Resolve(expr)
			if err != nil {
				return nil, err
			}
			newArgs = append(newArgs, newArg)
		}
		node.Args = newArgs
		return node, nil

	case Grouping:
		node := node.(Grouping)
		newExpr, err := res.Resolve(node.Expr)
		if err != nil {
			return nil, err
		}
		node.Expr = newExpr
		return node, nil

	case Num, Str, Bool, Nil:
		return node, nil

	default:
		return nil, fmt.Errorf("Couldn't resolve AST node with type %s: unknown node type", t)
	}
}

func (res *Resolver) beginScope() {
	res.scopes = append(res.scopes, map[string]bool{})
}

func (res *Resolver) endScope() {
	res.scopes = res.scopes[:len(res.scopes)-1]
}

func (res *Resolver) declare(name string) error {
	l := len(res.scopes)
	if l == 0 {
		return nil
	}
	_, ok := res.scopes[l-1][name]
	if ok {
		return fmt.Errorf("Tried to declare variable named %s more than once.", name)
	}
	res.scopes[l-1][name] = false
	return nil
}

func (res *Resolver) define(name string) {
	l := len(res.scopes)
	if l == 0 {
		return
	}
	res.scopes[l-1][name] = true
}

func (res *Resolver) resolveLocal(node Identifier) Identifier {
	for i := len(res.scopes) - 1; i >= 0; i-- {
		_, ok := res.scopes[i][node.Name]
		if ok {
			node.Depth = len(res.scopes) - 1 - i
		}
	}
	return node
}

func (res *Resolver) resolveThis(this This) This {
	for i := len(res.scopes) - 1; i >= 0; i-- {
		_, ok := res.scopes[i]["this"]
		if ok {
			this.Depth = len(res.scopes) - 1 - i
		}
	}
	return this
}
