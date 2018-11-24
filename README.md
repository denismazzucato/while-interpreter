# WhileInterpreter

Haskell Interpreter for the Denotational Semantic of **While**+ language.

Academic project for Software Verfication course, University of Padova.

## Installation

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/README/) (version 1.7.1 or newer)

On Unix System install as:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
Or
```bash
wget -qO- https://get.haskellstack.org/ | sh
```

## Get Started

- **While**+ is **While** plus some syntactic sugar for arithmetic/Boolean expressions and statements and including for and repeat-until loops.
- A program evaluated in this interpreter (in the initial *state* s0) respond as the same program applied to the denotational semantic function (in the same *state* s0). Thus, the semantic function is *undefined* iff the execution of the interpreter does not terminate, in the same program and initial *state*.
- This Interpreter relies on *Kleene-Knaster-Tarski* fixpoint iteration sequence for evaluating the while statements.
- To parse the input program I implemented the [wiki.haskell.org](https://wiki.haskell.org/Parsing_a_simple_imperative_language)'s tutorial.

## How to avoid run-time error in non-termination

To avoid run-time errors, in particular Stack Overflow errors, computing fixpoint iteration sequence, not necessary finite, I've used [Tail Recursion Optimization](https://wiki.haskell.org/Tail_recursion) (also known as Tail Recursion Elimination).

This is a (simple and not formal) proof of non-termination in non-termination statements evaluation, like `while true do skip`.

From the Haskell article:
> Since in a tail call, the containing function is about to return, its environment can actually be discarded and the recursive call can be entered without creating a new stack frame.

Implies that a function tail recursive don't build up a large stack of calls to itself, which wastes memory. In fact is enough to prove that the recursive function *least upper bound* `lub :: [(State -> State)] -> State -> State` is tail recursive to avoid run-time error for each program does not terminate.

lub is defined as:
```haskell
lub (g:gs) s
  | (g s /= Undef) = g s
  | otherwise = lub gs s
```
Where `(g:gs)` is the (possible) infinite list of the n-th application of the functional F at bottom, for each n in N, Natural numbers.
`(g:gs)` take values from a list comprehension that generate a possible infinite list, haskell compute elements on this list only when necessary, see [lazyness](https://wiki.haskell.org/Performance/Laziness).

`lub` define two branches, in this case I take only the recursive branch, I use `lub' (g:gs) = lub' gs` instead of `lub` definition, this implies `lub'` is tail recursive iff `lub` is tail recursive, because `lub'` is the `lub` recursive branching.

From the definition of tail recursive function, `lub'` is tail recursive iff `lub'` occurs tail recursively in his body, `lub'` occurs tail recursively in his body iff `lub'` occurs in his body (true) and holds that the body is in the form `t0 t1` and `lub'` occurs tail recursively in `t0`, since `t0` is `lub'`, and does not occur in `t1` (true).