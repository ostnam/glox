package eval

import (
    "time"
)

var globals = map[string]Ast{
	"clock": BuiltinFn{
		CallFn: func(val []Ast) (Ast, error) {
			return Num{
				Val: float64(time.Now().UnixMilli()),
			}, nil
		},
		ArityVal: 0,
	},
}
