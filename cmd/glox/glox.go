package main

import (
    "bufio"
    "fmt"
    "os"
    "github.com/ostnam/glox/pkg/scanner"
)

func runFile(path string) error {
    content, err := os.ReadFile(path)
    if err != nil {
        return err
    }
    run(content)
    return nil
}

func runRepl() {
    scanner := bufio.NewScanner(os.Stdin)
    for {
        fmt.Print("> ")
        eof := scanner.Scan()
        if !eof { // on ctrl-D, err is EOF
            fmt.Print("\n")
            return
        }
        run(scanner.Bytes())
    }
}

func run(input []byte) {
    runes := []rune(string(input[:]))
    toks, errs := scanner.Scan(runes)
    if len(errs) > 0 {
        for _, err := range errs {
            fmt.Println(err)
        }
    } else {
        for _, tok := range toks {
            fmt.Println(tok)
        }
    }
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
