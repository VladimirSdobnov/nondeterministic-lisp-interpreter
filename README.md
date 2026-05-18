# Nondeterministic Lisp Interpreter in Haskell

## Overview

This project is an educational implementation of a Lisp-like interpreter written in Haskell.  
The interpreter evolved from a classic deterministic evaluator into a nondeterministic evaluator inspired by Chapter 4.3 of the book:

> *Structure and Interpretation of Computer Programs* (SICP)

The project demonstrates:

- lexical analysis and parsing
- AST construction
- deterministic evaluation
- closures and lexical scoping
- continuations
- continuation-passing style (CPS)
- nondeterministic computation
- automatic backtracking
- constraint solving using `amb` and `require`

The interpreter was developed as part of a diploma project focused on nondeterministic computation models and declarative programming.

---

# Installation and Launch

## Requirements

Before running the project, install:

- GHC
- Cabal

Check installation:

```bash
ghc --version
cabal --version
```

## Clone Repository

```bash
git clone https://github.com/VladimirSdobnov/nondeterministic-lisp-interpreter.git
cd <project-folder>
```

## Build Project

```bash
cabal update
cabal build
```

## Run Interpreter

```bash
cabal run
```

# Architecture

The project is divided into several logical modules.

## Parser

Responsible for:

- tokenization
- parsing
- syntax validation
- AST construction

### Supported syntax

```scheme
(+ 1 2)
(if (> x 0) 1 0)
(lambda (x) (* x x))
(begin ...)
```

---

## AST

Defines the internal syntax tree representation:

```haskell
data Expr
    = Number Integer
    | Boolean Bool
    | Symbol String
    | List [Expr]
```

---

## Value System

Runtime values are represented through:

```haskell
data Value
    = NumberV Integer
    | BooleanV Bool
    | SymbolV String
    | ListV [Value]
    | PrimitiveFunc ...
    | Closure ...
```

The interpreter supports:

- primitive functions
- user-defined functions
- closures
- symbolic data

---

## Environment

Variable bindings are stored in immutable environments:

```haskell
type Env = Map.Map String Value
```

Environment updates return new environments instead of mutating existing ones.

---

# Deterministic Evaluator

The first stage of the project implements a traditional Lisp evaluator.

## Supported Features

### Arithmetic

```scheme
(+ 1 2 3)
(* 2 3 4)
(- 10 5)
(/ 20 2)
```

### Comparisons

```scheme
(= 1 1)
(< 2 3)
(> 10 5)
```

### Boolean Logic

```scheme
(and #t #t)
(or #f #t)
(not #f)
```

### Variables

```scheme
(define x 42)
```

### Functions

```scheme
(define square
    (lambda (x)
        (* x x)))
```

### Recursion

```scheme
(define factorial
    (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))
```

### Lists

```scheme
(list 1 2 3)
(cons 1 (list 2 3))
(car (list 1 2 3))
(cdr (list 1 2 3))
```

### Symbolic Data

```scheme
(quote (+ 1 2))
```

---

# CPS Evaluator

The second stage introduces a continuation-passing style evaluator.

Instead of returning values directly:

```haskell
eval :: Env -> Expr -> (Value, Env)
```

the evaluator uses continuations:

```haskell
eval
    :: Env
    -> Expr
    -> SuccessCont
    -> FailureCont
    -> IO ()
```

This architecture enables:

- explicit control flow
- delayed computations
- saved execution states
- backtracking
- nondeterministic search

---

# Nondeterministic Evaluation

The project implements a nondeterministic evaluator inspired by SICP.

## amb

Creates nondeterministic choice points:

```scheme
(amb 1 2 3)
```

The evaluator automatically explores alternatives.

---

## require

Prunes invalid execution branches:

```scheme
(require (> x 3))
```

If the condition fails:

- the current branch is abandoned
- the evaluator automatically backtracks
- another alternative is explored

---

## try-again

Allows the evaluator to search for additional valid solutions.

---

# Automatic Backtracking

Backtracking is implemented using failure continuations.

When a branch fails:

1. the current computation is abandoned
2. the evaluator returns to the latest `amb`
3. the next alternative is evaluated

This creates a search tree explored through depth-first search.

---

# Example: Multiple Dwelling Puzzle

The interpreter can solve logic problems declaratively.

Example:

```scheme
(begin

    (define baker (amb 1 2 3 4))
    (define cooper (amb 2 3 4 5))
    (define fletcher (amb 2 3 4))
    (define miller (amb 1 2 3 4 5))
    (define smith (amb 1 2 3 4 5))

    (require
        (distinct?
            (list
                baker
                cooper
                fletcher
                miller
                smith)))

    (require (> miller cooper))

    ...)
```

The program describes constraints rather than a search algorithm.

The evaluator itself performs:

- search
- branching
- backtracking
- solution discovery

---

# Optimization Notes

Nondeterministic evaluation can lead to combinatorial explosion.

The project demonstrates several optimization strategies:

- early pruning
- reduced search domains
- constraint ordering
- incremental filtering

---

# Educational Value

The project demonstrates:

- interpreter construction
- parser implementation
- immutable environments
- closures and lexical scope
- continuation-passing style
- nondeterministic computation
- declarative programming
- symbolic computation
- automatic search and backtracking

---

# Technologies

- Haskell
- GHC
- Continuation-Passing Style (CPS)
- Immutable Data Structures
- Functional Programming

---

# References

1. Harold Abelson, Gerald Jay Sussman — *Structure and Interpretation of Computer Programs*
2. Simon Thompson — *Haskell: The Craft of Functional Programming*
3. Graham Hutton — *Programming in Haskell*

---

# Future Improvements

Possible future extensions:

- lazy search strategies
- optimization heuristics
- tail-call optimization
- macro system
- pattern matching
- Prolog-like rules
- constraint propagation
- SAT-style pruning
- typed runtime values

---

# Author: Vladimir Sdobvnov is a student at UNN

Diploma project on nondeterministic computation models and Lisp interpreter architecture.
