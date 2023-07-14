package parser

import (
	"fmt"
	"strconv"

	"github.com/ostnam/glox/pkg/ast"
	. "github.com/ostnam/glox/pkg/tokens"
	"github.com/ostnam/glox/pkg/utils"
)

// Top-level parsing function
func Parse(tokens []Token) ([]ast.Ast, []error) {
    pos := 0
    res := []ast.Ast{}
    errs := []error{}
    if tokens[len(tokens) - 1].Type == EOF {
        tokens = tokens[:len(tokens) - 1]
    }
    for !utils.IsAtEnd(tokens, pos) {
        node, err := parseExpr(tokens, &pos)
        if node != nil {
            res = append(res, node)
        }
        if err != nil {
            errs = append(errs, err)
        }
    }
    return res, errs
}

func parseExpr(tokens []Token, pos *int) (ast.Ast, error) {
    return parseEquality(tokens, pos)
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
            Op: ast.TokToBinop[op.Type],
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
            Op: ast.TokToBinop[op.Type],
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
            Op: ast.TokToBinop[op.Type],
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
        expr = ast.Binop {
            Op: ast.TokToBinop[op.Type],
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
        expr := ast.Unop {
            Op: ast.TokToUnop[op.Type],
            Val: val,
        }
        return expr, nil
    }
    return parsePrimary(tokens, pos)
}

func parsePrimary(tokens []Token, pos *int) (ast.Ast, error) {
    if utils.MatchTokenType(tokens, pos, False) {
        return ast.Bool { Val: false }, nil
    }
    if utils.MatchTokenType(tokens, pos, True) {
        return ast.Bool { Val: true }, nil
    }
    if utils.MatchTokenType(tokens, pos, Nil) {
        return ast.Nil {}, nil
    }
    if utils.MatchTokenType(tokens, pos, Str) {
        return ast.Str {Val: utils.Previous(tokens, *pos).Literal}, nil
    }
    if utils.MatchTokenType(tokens, pos, Num) {
        prev := utils.Previous(tokens, *pos).Literal
        val, err := strconv.ParseInt(prev, 10, 64)
        if err != nil {
            return nil, fmt.Errorf("BUG: Internal parse error, couldn't parse int from int literal: %s", prev)
        }
        return ast.Int {Val: val }, nil
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
        return ast.Grouping { Expr: expr }, nil
    }
    return nil, fmt.Errorf("Error parsing primary value")
}

// Advances the parsing state until the probable beginning of the next
// statement, or the end of the token stream.
func synchronize(tokens []Token, pos *int) {
    utils.Advance(tokens, pos)
    for !utils.IsAtEnd(tokens, *pos) {
        if utils.Previous(tokens, *pos).Type == Semicolon {
            return
        }
        if utils.PeekMatchesTokType(tokens, *pos, Class, Fun, For, If, While, Print, Return) {
            return
        }
        utils.Advance(tokens, pos)
    }
}
