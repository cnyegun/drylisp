# DryLisp

A small Lisp interpreter written in Haskell. This was a learning project to understand how parser and interpreters work.

## What It Does

DryLisp can evaluate basic Lisp expressions including:

- **Literals**: numbers, strings, booleans (`true`/`false`)
- **Variables**: looked up in an environment
- **Conditionals**: `if` expressions
- **Lambdas**: anonymous functions with closures
- **Quoting**: prevent evaluation with `quote` or `'`
- **Basic primitives**: `+`, `-`, `*`, `cons`, `car`, `cdr`

## Example Usage

```haskell
import DryLisp
import LispParser

main :: IO ()
main = do
    let input = "(+ 1 2 3)"
    case parse lispExprP input of
        Just (expr, _) -> print $ eval initialEnv expr
        Nothing        -> putStrLn "Parse error"
```

## The Eval Function

The `eval` function pattern-matches on expression types:

- **Atoms** (strings, numbers, booleans) evaluate to themselves
- **Identifiers** are looked up in the environment
- **Lists** are treated as function applications, special forms, or macros

Closures capture their defining environment, so this works as expected:

```lisp
((lambda (x)
    (lambda (y) 
        (+ x y))) 
  5)
```

## The Parser

The parser is built from scratch using applicative functor combinators. No external parsing libraries it just `Functor`, `Applicative`, and `Alternative`.

It handles:
- Numbers
- Strings
- Booleans
- Identifiers (names)
- Lists
- Quoting

## License

GNU GPLv3