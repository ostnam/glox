package parser

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/ostnam/glox/pkg/eval"
	. "github.com/ostnam/glox/pkg/tokens"
	"github.com/ostnam/glox/pkg/utils"
)

// Top-level parsing function.
// Values in the returned slices are never nil.
func Parse(tokens []Token) ([]eval.Ast, []error) {
	pos := 0            // index of the next token to parse
	res := []eval.Ast{} // every AST node parsed
	errs := []error{}   // every error collected
	// main parsing loop
	for !utils.IsAtEnd(tokens, pos) && !utils.PeekMatchesTokType(tokens, pos, EOF) {
		node, err := parseDeclaration(tokens, &pos)
		if node != nil {
			res = append(res, node)
		}
		if err != nil {
			errs = append(errs, err)
		}
	}
	return res, errs
}

func parseDeclaration(tokens []Token, pos *int) (eval.Ast, error) {
	next := utils.Peek(tokens, *pos)
	if next == nil {
		return nil, errors.New("Couldn't parse declaration")
	}
	if next.Type == Var {
		return parseVarDecl(tokens, pos)
	}
	if next.Type == Fun {
		utils.Advance(tokens, pos)
		return parseFunDecl(tokens, pos)
	}
	if utils.MatchTokenType(tokens, pos, Class) {
		return parseClassDecl(tokens, pos)
	}
	return parseStatement(tokens, pos)
}

func parseClassDecl(tokens []Token, pos *int) (eval.Ast, error) {
	className := utils.Advance(tokens, pos)
	if className.Type != Identifier {
		return nil, fmt.Errorf("Expected class name, instead got the token: %v", className)
	}
	if !utils.MatchTokenType(tokens, pos, LeftBrace) {
		return nil, fmt.Errorf("Missing { after declaring class name.")
	}
	methods := []eval.Fn{}
	for !utils.PeekMatchesTokType(tokens, *pos, RightBrace) && !utils.IsAtEnd(tokens, *pos) {
		method, err := parseFunDecl(tokens, pos)
		if err != nil {
			return nil, err
		}
		fn, ok := method.(eval.FnDecl)
		if !ok {
			return nil, fmt.Errorf("INTERNAL BUG: parseFunDecl didn't return an FnDecl")
		}
		methods = append(methods, fn.AsFn(nil))
	}
	if !utils.MatchTokenType(tokens, pos, RightBrace) {
		return nil, fmt.Errorf("Missing } after declaring class methods.")
	}
	return eval.ClassDecl{Name: className.Lexeme, Methods: methods}, nil
}

// Parsed a variable declaration of the form var x; or var x = 10;
func parseVarDecl(tokens []Token, pos *int) (eval.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil {
		return nil, errors.New("Couldn't parse variable declaration")
	}
	if fst.Type != Var {
		return nil, fmt.Errorf("Couldn't parse variable declaration: first token should be the keyword var but instead is: %v", fst)
	}
	name := utils.Advance(tokens, pos)
	if name == nil {
		return nil, errors.New("Couldn't parse variable declaration")
	}
	if name.Type != Identifier {
		return nil, fmt.Errorf("Syntax error: expected identifier after keyword var, got: %v", name)
	}

	if utils.MatchTokenType(tokens, pos, Semicolon) {
		return eval.VarDecl{
			Name: eval.Identifier{Name: name.Literal, Depth: -1},
		}, nil
	}

	eql := utils.Peek(tokens, *pos)
	if eql == nil || eql.Type != Eql {
		return nil, fmt.Errorf("Syntax error: expected = for var initialization, got: %v", eql)
	}
	utils.Advance(tokens, pos)

	expr, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	if utils.MatchTokenType(tokens, pos, Semicolon) {
		return eval.VarInit{
			Name: eval.Identifier{Name: name.Literal, Depth: -1},
			Val:  expr,
		}, nil
	}
	return nil, fmt.Errorf("Syntax error: initialization of variable doesn't end with a ;")
}

// The "fun" keyword should have been matched before this function is called.
func parseFunDecl(tokens []Token, pos *int) (eval.Ast, error) {
	name := utils.Advance(tokens, pos)
	if name == nil || name.Type != Identifier {
		return nil, fmt.Errorf("Error parsing name of function declaration.")
	}
	if !utils.MatchTokenType(tokens, pos, LeftParen) {
		return nil, fmt.Errorf("Function named %s not followed by (.", name.Lexeme)
	}
	params, err := parseParams(tokens, pos)
	if err != nil {
		return nil, err
	}
	if len(params) >= 255 {
		return nil, fmt.Errorf("Tried to define a function named %s which takes %d parameters, which is over the limit of 254", name.Lexeme, len(params))
	}

	if !utils.MatchTokenType(tokens, pos, RightParen) {
		return nil, fmt.Errorf("Function named %s param list not followed by ).", name.Lexeme)
	}
	body, err := parseBlock(tokens, pos)
	if err != nil {
		return nil, err
	}
	return eval.FnDecl{
		Fn: eval.Fn{
			Name:    name.Lexeme,
			Params:  params,
			Body:    body,
			Closure: nil,
		},
	}, nil
}

func parseParams(tokens []Token, pos *int) ([]string, error) {
	res := []string{}
	for utils.PeekMatchesTokType(tokens, *pos, Identifier) {
		res = append(res, utils.Advance(tokens, pos).Lexeme)
		if utils.PeekMatchesTokType(tokens, *pos, RightParen) {
			break
		}
		if !utils.MatchTokenType(tokens, pos, Comma) {
			return nil, fmt.Errorf("Error parsing parameters of function, expected a comma after argument but got %v", utils.Peek(tokens, *pos))
		}
	}
	return res, nil
}

func parseStatement(tokens []Token, pos *int) (eval.Ast, error) {
	next := utils.Peek(tokens, *pos)
	if next == nil {
		return nil, errors.New("Couldn't parse statement")
	}
	if next.Type == Print {
		return parsePrintStatement(tokens, pos)
	} else if next.Type == LeftBrace {
		return parseBlock(tokens, pos)
	} else if next.Type == If {
		return parseIfStatement(tokens, pos)
	} else if next.Type == While {
		return parseWhileStatement(tokens, pos)
	} else if next.Type == For {
		return parseForStatement(tokens, pos)
	} else if next.Type == Return {
		return parseReturnStatement(tokens, pos)
	} else {
		return parseExprStatement(tokens, pos)
	}
}

func parseReturnStatement(tokens []Token, pos *int) (eval.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil || fst.Type != Return {
		return nil, errors.New("Return statement parsing error.")
	}
	if utils.MatchTokenType(tokens, pos, Semicolon) {
		return eval.ReturnStmt{Val: nil}, nil
	}
	expr, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	if !utils.MatchTokenType(tokens, pos, Semicolon) {
		return nil, fmt.Errorf("Error parsing return statement: doesn't end with a semicolon")
	}
	return eval.ReturnStmt{Val: expr}, nil
}

func parseForStatement(tokens []Token, pos *int) (eval.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil || fst.Type != For {
		return nil, errors.New("For statement parsing error.")
	}
	open := utils.Advance(tokens, pos)
	if open == nil || open.Type != LeftParen {
		return nil, errors.New("Error parsing for statement")
	}
	var init eval.Ast = nil
	if utils.PeekMatchesTokType(tokens, *pos, Var) {
		val, err := parseVarDecl(tokens, pos)
		if err != nil {
			return nil, err
		}
		init = val
	} else if !utils.PeekMatchesTokType(tokens, *pos, Semicolon) {
		val, err := parseExprStatement(tokens, pos)
		if err != nil {
			return nil, err
		}
		init = val
	} else {
		ok := utils.MatchTokenType(tokens, pos, Semicolon)
		if !ok {
			return nil, fmt.Errorf("Incorrect for loop init")
		}
	}

	var cond eval.Ast = nil
	if !utils.PeekMatchesTokType(tokens, *pos, Semicolon) {
		val, err := parseExpr(tokens, pos)
		if err != nil {
			return nil, err
		}
		cond = val
	}
	if !utils.MatchTokenType(tokens, pos, Semicolon) {
		return nil, errors.New("No semicolon at the end of a for condition.")
	}

	var update eval.Ast = nil
	if !utils.PeekMatchesTokType(tokens, *pos, RightParen) {
		val, err := parseExpr(tokens, pos)
		if err != nil {
			return nil, err
		}
		update = val
	}
	if !utils.MatchTokenType(tokens, pos, RightParen) {
		return nil, errors.New("No ) at the end of a for loop.")
	}

	stmt, err := parseStatement(tokens, pos)
	if err != nil {
		return nil, err
	}

	body := eval.Block{
		Statements: []eval.Ast{stmt},
	}
	if update != nil {
		body.Statements = append(body.Statements, eval.Stmt{Kind: eval.ExprStmt, Expr: update})
	}
	while := eval.While{
		Pred: eval.Bool{Val: true},
		Body: body,
	}
	if cond != nil {
		while.Pred = cond
	}
	if init != nil {
		return eval.Block{
			Statements: []eval.Ast{init, while},
		}, nil
	}
	return while, nil
}

func parseWhileStatement(tokens []Token, pos *int) (eval.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil || fst.Type != While {
		return nil, errors.New("While statement parsing error.")
	}
	open := utils.Advance(tokens, pos)
	if open == nil || open.Type != LeftParen {
		return nil, errors.New("Error parsing while statement")
	}
	pred, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	closing := utils.Advance(tokens, pos)
	if closing == nil || closing.Type != RightParen {
		return nil, errors.New("Error parsing while statement")
	}
	body, err := parseStatement(tokens, pos)
	if err != nil {
		return nil, err
	}
	return eval.While{Pred: pred, Body: body}, nil
}

// Tries to parse an if statement, consuming tokens if failing.
func parseIfStatement(tokens []Token, pos *int) (eval.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil || fst.Type != If {
		return nil, errors.New("If statement parsing error.")
	}
	open := utils.Advance(tokens, pos)
	if open == nil || open.Type != LeftParen {
		return nil, errors.New("Error parsing if statement")
	}
	pred, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	closing := utils.Advance(tokens, pos)
	if closing == nil || closing.Type != RightParen {
		return nil, errors.New("Error parsing if statement")
	}
	body, err := parseStatement(tokens, pos)
	if err != nil {
		return nil, err
	}
	if utils.MatchTokenType(tokens, pos, Else) {
		elseStmt, err := parseStatement(tokens, pos)
		if err != nil {
			return nil, err
		}
		return eval.IfStmt{Pred: pred, Body: body, Else: elseStmt}, nil
	}
	return eval.IfStmt{Pred: pred, Body: body, Else: nil}, nil
}

func parsePrintStatement(tokens []Token, pos *int) (eval.Ast, error) {
	first := utils.Advance(tokens, pos)
	if first.Type != Print {
		return nil, fmt.Errorf("Error parsing print expression, first token %v isn't print.", first)
	}
	expr, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	last := utils.Advance(tokens, pos)
	if last.Type != Semicolon {
		return nil, fmt.Errorf("Print statement doesn't end with a semicolon, but with the following token: %#v", last)
	}
	return eval.Stmt{Kind: eval.PrintStmt, Expr: expr}, nil
}

func parseBlock(tokens []Token, pos *int) (eval.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil {
		return nil, errors.New("Couldn't parse a block's beginning brace.")
	}
	body := []eval.Ast{}
	next := utils.Peek(tokens, *pos)
	for !utils.IsAtEnd(tokens, *pos) && next != nil && next.Type != RightBrace {
		decl, err := parseDeclaration(tokens, pos)
		if err != nil {
			return nil, err
		}
		body = append(body, decl)
		next = utils.Peek(tokens, *pos)
	}
	ok := utils.MatchTokenType(tokens, pos, RightBrace)
	if !ok {
		return nil, fmt.Errorf("Block doesn't end with a }")
	}
	return eval.Block{Statements: body}, nil
}

func parseExprStatement(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	last := utils.Advance(tokens, pos)
	if last.Type != Semicolon {
		return nil, fmt.Errorf("Expression statement doesn't end with a semicolon, but with the following token: %v", last)
	}
	return eval.Stmt{Kind: eval.ExprStmt, Expr: expr}, nil
}

func parseExpr(tokens []Token, pos *int) (eval.Ast, error) {
	return parseAssignment(tokens, pos)
}

func parseAssignment(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseLogicOr(tokens, pos)
	if err != nil {
		return nil, err
	}
	if utils.MatchTokenType(tokens, pos, Eql) {
		rhs, err := parseAssignment(tokens, pos)
		if err != nil {
			return nil, err
		}
		id, ok := expr.(eval.Identifier)
		if ok {
			return eval.Assignment{
				Name: id,
				Val:  rhs,
			}, nil
		}
		get, ok := expr.(eval.Get)
		if ok {
			return eval.Set{
				Lhs:       get.Lhs,
				FieldName: get.FieldName,
				Val:       rhs,
			}, nil
		}
		return nil, fmt.Errorf("Incorrect left-hand side of assignment: %v", expr)
	}
	return expr, nil
}

func parseLogicOr(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseLogicAnd(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.MatchTokenType(tokens, pos, Or) {
		rhs, err := parseLogicAnd(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr = eval.Or{Lhs: expr, Rhs: rhs}
	}

	return expr, nil
}

func parseLogicAnd(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseEquality(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.MatchTokenType(tokens, pos, And) {
		rhs, err := parseEquality(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr = eval.And{Lhs: expr, Rhs: rhs}
	}

	return expr, nil
}

func parseEquality(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseComparison(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.MatchTokenType(tokens, pos, Bang, BangEql) {
		op := utils.Previous(tokens, *pos)
		if op == nil {
			return nil, fmt.Errorf("Couldn't parse equality")
		}
		right, err := parseComparison(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr = eval.Binop{
			Op:  eval.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseComparison(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseTerm(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.MatchTokenType(tokens, pos, Greater, GreaterEql, Less, LessEql) {
		op := utils.Previous(tokens, *pos)
		if op == nil {
			return nil, fmt.Errorf("Couldn't parse equality")
		}
		right, err := parseTerm(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr = eval.Binop{
			Op:  eval.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseTerm(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseFactor(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.MatchTokenType(tokens, pos, Minus, Plus) {
		op := utils.Previous(tokens, *pos)
		if op == nil {
			return nil, fmt.Errorf("Couldn't parse equality")
		}
		right, err := parseTerm(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr = eval.Binop{
			Op:  eval.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseFactor(tokens []Token, pos *int) (eval.Ast, error) {
	expr, err := parseUnary(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.MatchTokenType(tokens, pos, Slash, Star) {
		op := utils.Previous(tokens, *pos)
		if op == nil {
			return nil, fmt.Errorf("Couldn't parse equality")
		}
		right, err := parseTerm(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr = eval.Binop{
			Op:  eval.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseUnary(tokens []Token, pos *int) (eval.Ast, error) {
	for utils.MatchTokenType(tokens, pos, Bang, Minus) {
		op := utils.Previous(tokens, *pos)
		val, err := parseUnary(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr := eval.Unop{
			Op:  eval.TokToUnop[op.Type],
			Val: val,
		}
		return expr, nil
	}
	return parseFnCall(tokens, pos)
}

func parseFnCall(tokens []Token, pos *int) (eval.Ast, error) {
	var maxNumArgsExceeded error = nil
	expr, err := parsePrimary(tokens, pos)
	if err != nil {
		return nil, err
	}
	for utils.PeekMatchesTokType(tokens, *pos, LeftParen, Dot) {
		if utils.MatchTokenType(tokens, pos, LeftParen) {
			args := []eval.Ast{}
			if !utils.PeekMatchesTokType(tokens, *pos, RightParen) {
				args, err = parseArgumentList(tokens, pos)
				if err != nil {
					return nil, err
				}
				if len(args) >= 255 {
					maxNumArgsExceeded = fmt.Errorf("Exceeded the maximum (254) number of arguments passed to a function")
				}
			}
			expr = eval.FnCall{Fn: expr, Args: args}
			if !utils.MatchTokenType(tokens, pos, RightParen) {
				return nil, errors.New("Error parsing function call")
			}
		} else if utils.MatchTokenType(tokens, pos, Dot) {
			fieldName := utils.Advance(tokens, pos)
			if fieldName.Type != Identifier {
				return nil, fmt.Errorf("Incorrect field access.")
			}
			expr = eval.Get{Lhs: expr, FieldName: fieldName.Lexeme}
		}
	}
	return expr, maxNumArgsExceeded
}

// Returns an error if the argument list is empty
func parseArgumentList(tokens []Token, pos *int) ([]eval.Ast, error) {
	res := []eval.Ast{}
	fst, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	res = append(res, fst)
	for utils.MatchTokenType(tokens, pos, Comma) {
		expr, err := parseExpr(tokens, pos)
		if err != nil {
			return nil, err
		}
		res = append(res, expr)
	}
	return res, nil
}

func parsePrimary(tokens []Token, pos *int) (eval.Ast, error) {
	if utils.MatchTokenType(tokens, pos, This) {
		return eval.This{}, nil
	}
	if utils.MatchTokenType(tokens, pos, False) {
		return eval.Bool{Val: false}, nil
	}
	if utils.MatchTokenType(tokens, pos, True) {
		return eval.Bool{Val: true}, nil
	}
	if utils.MatchTokenType(tokens, pos, Nil) {
		return eval.Nil{}, nil
	}
	if utils.MatchTokenType(tokens, pos, Str) {
		return eval.Str{Val: utils.Previous(tokens, *pos).Literal}, nil
	}
	if utils.MatchTokenType(tokens, pos, Num) {
		prev := utils.Previous(tokens, *pos).Literal
		val, err := strconv.ParseFloat(prev, 64)
		if err != nil {
			return nil, fmt.Errorf("BUG: Internal parse error, couldn't parse int from int literal: %s", prev)
		}
		return eval.Num{Val: val}, nil
	}
	if utils.MatchTokenType(tokens, pos, LeftParen) {
		expr, err := parseExpr(tokens, pos)
		if err != nil {
			return nil, err
		}
		next := utils.Advance(tokens, pos)
		if next == nil {
			return nil, fmt.Errorf("Error parsing between parens")
		}
		if next.Type != RightParen {
			return nil, fmt.Errorf("Unclosed parentheses")
		}
		return eval.Grouping{Expr: expr}, nil
	}
	if utils.MatchTokenType(tokens, pos, Identifier) {
		prev := utils.Previous(tokens, *pos)
		return eval.Identifier{Name: prev.Literal, Depth: -1}, nil
	}
	// This is the bottom of the parser, if we reach this line, nothing has
	// matched. To avoid going into an endless loop, we synchronize, otherwise
	// we'll restart the parsing at the same position.
	synchronize(tokens, pos)
	return nil, fmt.Errorf("Error parsing primary value")
}

// Advances the parsing state until the probable beginning of the next
// statement, or the end of the token stream.
// Will never consume the EOF token.
func synchronize(tokens []Token, pos *int) {
	for {
		if utils.IsAtEnd(tokens, *pos) {
			return
		}
		next := utils.Peek(tokens, *pos)
		if next == nil {
			return
		}
		if next.Type == EOF {
			return
		}
		if next.Type == Semicolon {
			utils.Advance(tokens, pos)
			return
		}
		if utils.PeekMatchesTokType(tokens, *pos, Class, Fun, For, If, While, Print, Return) {
			return
		}
		utils.Advance(tokens, pos)
	}
}
