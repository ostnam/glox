package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/ostnam/glox/pkg/eval"
	"github.com/ostnam/glox/pkg/parser"
	"github.com/ostnam/glox/pkg/scanner"
)

// Run the file at the given path as a lox program.
func runFile(path string) error {
	content, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	run(content, true)
	return nil
}

// Runs the REPL
func runRepl() {
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		eof := scanner.Scan()
		if !eof { // on ctrl-D, err is EOF
			fmt.Print("\n")
			return
		}
		run(scanner.Bytes(), true)
	}
}

// Evaluates the slice of bytes passed as input.
// Return value is whether the evaluation returned successfully.
func run(input []byte, debug bool) bool {
	runes := []rune(string(input))
	toks, errs := scanner.Scan(runes)
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
			return false
		}
	}
	if debug {
		fmt.Println("Tokens scanned:")
		for _, tok := range toks {
			fmt.Printf("  %#v\n", tok)
		}
	}

	exprs, errs := parser.Parse(toks)
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Println(err)
			return false
		}
	}
	if debug {
		fmt.Println("\nAST parsed:")
		for _, node := range exprs {
			eval.PrettyPrintAst(node)
		}
	}

	interpreter := eval.NewInterpreter()
	for _, expr := range exprs {
		val, err := interpreter.Eval(expr)
		if err != nil {
			fmt.Println(err)
			return false
		}
		if debug {
			fmt.Printf("%#v\n", val)
		}
	}
	return true
}

func printErr(line int, where string, msg string) {
	fmt.Printf("Line %d, at \"%s\": %s", line, where, msg)
}

func main() {
	args := os.Args[1:]
	n_args := len(args)
	if n_args > 1 {
		fmt.Println("usage: glox SOURCE_FILE_PATH")
		os.Exit(64)
	} else if n_args == 1 {
		runFile(args[0])
	} else {
		runRepl()
	}
}
