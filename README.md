# Lambda Calculus Bytecode Interpreter

Was originally made for a school assignment, we had to design a flowchart, hence the name "flowchart" being occasionally used for this project.

This was made mainly for learning how to design bytecode.

Unfortunately, due to the bottom-up and strict evaluation, the Y-Combinator is not encodable as it results in an infinite recursion.

## Running

The program needs access to a `std.lc` / `std.lc.o` file. Specified through `LC_IMPORT_PATH`.

On nix, it is automatically solved in the executable made by the flake

## Terms

- `import "file"` makes visible the symbols defined in 'file'
- `<name> = <expr>` defines a symbol

## Utility

- `trace` prints the passed value and returns it

```
> +1 (trace 2)
2
3
```

- `read` takes a single character from stdin and converts the ascii value to a church numeral

## REPL

| command | description                    |
| ------- | ------------------------------ |
| `:q`    | exit the repl                  |
| `:r`    | reloads all files that changed |
| `:pwd`  | gets current directory         |
| `:cd`   | change current directory       |
| `:`     | repeat last command            |

