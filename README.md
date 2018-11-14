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