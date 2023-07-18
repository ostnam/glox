package parser

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/ostnam/glox/pkg/ast"
	. "github.com/ostnam/glox/pkg/tokens"
	"github.com/ostnam/glox/pkg/utils"
)

// Top-level parsing function.
// Values in the returned slices are never nil.
func Parse(tokens []Token) ([]ast.Ast, []error) {
	pos := 0           // index of the next token to parse
	res := []ast.Ast{} // every AST node parsed
	errs := []error{}  // every error collected
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

func parseDeclaration(tokens []Token, pos *int) (ast.Ast, error) {
	next := utils.Peek(tokens, *pos)
	if next == nil {
		return nil, errors.New("Couldn't parse declaration")
	}
	if next.Type == Var {
		return parseVarDecl(tokens, pos)
	}
	return parseStatement(tokens, pos)
}

// Parsed a variable declaration of the form var x; or var x = 10;
func parseVarDecl(tokens []Token, pos *int) (ast.Ast, error) {
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
		return ast.VarDecl{
			Name: ast.Identifier{Name: name.Literal},
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
		return ast.VarInit{
			Name: ast.Identifier{Name: name.Literal},
			Val:  expr,
		}, nil
	}
	return nil, fmt.Errorf("Syntax error: initialization of variable doesn't end with a ;")
}

func parseStatement(tokens []Token, pos *int) (ast.Ast, error) {
	next := utils.Peek(tokens, *pos)
	if next == nil {
		return nil, errors.New("Couldn't parse statement")
	}
	if next.Type == Print {
		return parsePrintStatement(tokens, pos)
	} else if next.Type == LeftBrace {
		return parseBlock(tokens, pos)
	} else {
		return parseExprStatement(tokens, pos)
	}
}

func parsePrintStatement(tokens []Token, pos *int) (ast.Ast, error) {
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
	return ast.Stmt{Kind: ast.PrintStmt, Expr: expr}, nil
}

func parseBlock(tokens []Token, pos *int) (ast.Ast, error) {
	fst := utils.Advance(tokens, pos)
	if fst == nil {
		return nil, errors.New("Couldn't parse a block's beginning brace.")
	}
	body := []ast.Ast{}
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
	return ast.Block{Statements: body}, nil
}

func parseExprStatement(tokens []Token, pos *int) (ast.Ast, error) {
	expr, err := parseExpr(tokens, pos)
	if err != nil {
		return nil, err
	}
	last := utils.Advance(tokens, pos)
	if last.Type != Semicolon {
		return nil, fmt.Errorf("Expression statement doesn't end with a semicolon, but with the following token: %v", last)
	}
	return ast.Stmt{Kind: ast.ExprStmt, Expr: expr}, nil
}

func parseExpr(tokens []Token, pos *int) (ast.Ast, error) {
	return parseAssignment(tokens, pos)
}

func parseAssignment(tokens []Token, pos *int) (ast.Ast, error) {
	expr, err := parseEquality(tokens, pos)
	if err != nil {
		return nil, err
	}
	if utils.MatchTokenType(tokens, pos, Eql) {
		rhs, err := parseAssignment(tokens, pos)
		if err != nil {
			return nil, err
		}
		lhs, ok := expr.(ast.Identifier)
		if !ok {
			return nil, fmt.Errorf("Incorrect left hand side of assignment: %v", lhs)
		}
		return ast.Assignment{
			Name: lhs,
			Val:  rhs,
		}, nil
	}
	return expr, nil
}

func parseEquality(tokens []Token, pos *int) (ast.Ast, error) {
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
		expr = ast.Binop{
			Op:  ast.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseComparison(tokens []Token, pos *int) (ast.Ast, error) {
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
		expr = ast.Binop{
			Op:  ast.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseTerm(tokens []Token, pos *int) (ast.Ast, error) {
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
		expr = ast.Binop{
			Op:  ast.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseFactor(tokens []Token, pos *int) (ast.Ast, error) {
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
		expr = ast.Binop{
			Op:  ast.TokToBinop[op.Type],
			Lhs: expr,
			Rhs: right,
		}
	}
	return expr, nil
}

func parseUnary(tokens []Token, pos *int) (ast.Ast, error) {
	for utils.MatchTokenType(tokens, pos, Bang, Minus) {
		op := utils.Previous(tokens, *pos)
		val, err := parseUnary(tokens, pos)
		if err != nil {
			return nil, err
		}
		expr := ast.Unop{
			Op:  ast.TokToUnop[op.Type],
			Val: val,
		}
		return expr, nil
	}
	return parsePrimary(tokens, pos)
}

func parsePrimary(tokens []Token, pos *int) (ast.Ast, error) {
	if utils.MatchTokenType(tokens, pos, False) {
		return ast.Bool{Val: false}, nil
	}
	if utils.MatchTokenType(tokens, pos, True) {
		return ast.Bool{Val: true}, nil
	}
	if utils.MatchTokenType(tokens, pos, Nil) {
		return ast.Nil{}, nil
	}
	if utils.MatchTokenType(tokens, pos, Str) {
		return ast.Str{Val: utils.Previous(tokens, *pos).Literal}, nil
	}
	if utils.MatchTokenType(tokens, pos, Num) {
		prev := utils.Previous(tokens, *pos).Literal
		val, err := strconv.ParseFloat(prev, 64)
		if err != nil {
			return nil, fmt.Errorf("BUG: Internal parse error, couldn't parse int from int literal: %s", prev)
		}
		return ast.Num{Val: val}, nil
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
		return ast.Grouping{Expr: expr}, nil
	}
	if utils.MatchTokenType(tokens, pos, Identifier) {
		prev := utils.Previous(tokens, *pos)
		return ast.Identifier{Name: prev.Literal}, nil
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
