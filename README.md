# The Phage Programming Language

Phage is a functional, syntactically pure, interpreted programming language.
It aims to provide maximum expressivity, excellent support for metaprogramming,
and be 'hackable'.

## Features

### Redefine Everything

Phage has no keywords (you can redefine `if`, `true`, and `def` if you want). This is
because `if`, `true`, and `def` are normal variables.

```scheme
(def a 1)    // a becomes 1
(def d def)  // d becomes def
(d b 4)      // b becomes 4
(d def 4)    // def becomes 4
(def c 5)    // broken! you can't call 4!
```

Because of this property, it's very easy to project your personal tastes onto phage. In fact, the core language defines variables `def`, `define` and `var` to do the exact same thing. There are also two other ways to define variables, that act differently, `fun`, and `let`.

### Immutability

Data is immutable. When redefine variables, everything up to that point will
maintain the old reference. Here is an example (notice that when we redefine
`a`, `pr` mainrtains the old reference):

```scheme
(def a 1)              // a becomes 1
(print a)              // prints 1
(fun pr () (print a))  // defines a function that prints a
(def a 2)              // a becomes 2
(print a)              // prints 2
(pr)                   // prints 1
```

This is different from variable shadowing, which only occurs on nested scopes. In Phage, functions are bound to their contexts (everything defined above them), and ignore definitions below, and in more nested scopes than them.

### Syntactic Purity

* A program consists of many lists
* Lists are evaluated by taking the first element (the `car`), which should be a function / special form, and applying the tail (the `cdr`) to it.

## Usage

```bash
$ stack build                    # to build
$ stack exec phage               # run the repl
$ stack exec phage -- <program>  # run a phage program
```

## Examples

```scheme
(import "prelude/prelude.scm")

// ---
// Data Types
// ---

42              // Numbers
"Hello, World!" // Strings
print           // Functions
true            // Booleans

// ---
// Function Application
// ---

(+ 1 2)        // 3
(+ 1 2 3 4 5)  // 15

// ---
// Function Definition
// ---

// `hello` takes two parameters, prints them, then returns their sum
(fun hello (a b) (print a b) (+ a b))

// Anonymous functions are created with `\`
(\ (a b c) (+ a (- b c)))

// ---
// Currying / Partial Application
// ---

(def add3 (+ 3))
(add3 5)            // 8

// ---
// Here are some ways to define factorial
// ---

// normal, recursive way
(fun fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))

// product of range
(fun fact (a) (prod (upto a)))

// data pipe
(def fact (pipe upto prod))

// ---
// Lists
// ---

//       |-create list-|
(def lst (quote (1 2 3)))

// get first element
(car lst)    // 1

// get all other elements
(cdr lst)    // (2 3)

// get second element
(cdar lst)   // 2

// third
(cddar lst)  // 3

// let's operate on some nested lists
(def lst (quote ((1 (2 3) 4) 5 6)))

// head of head of list
(caar lst)   // 1

(cadar lst)  // (2 3)
(cadadr lst) // (3)

// etc.

// There are other functions defined in the prelude, such as len, fold, sum,
// prod, choose, minl, maxl, rev, last, all, any, filter, etc. I recommend
// reading their implementation in `prelude/prelude.scm`. They're all very
// simple

// ---
// Data Pipe
// ---

// Data pipes are similar to composition in functional languages, except that
// pipes create functions that can take more than one argument.

(def a (pipe (+ 1) (* 3) (+ 4)))
(a 5)        // 22

// `pipe` takes a list of functions, and returns a function.
// in the example above, `a` applies `5` to `(+ 1)` to get `6`,
// then applies `6` to `(* 3)`, and so on, returning the result. 

// Because (+ 1) can take multiple arguments, the result of the pipe can too

(a 2 3 5)    // 37

// in `pipe` the data flows left-to-right, but in `rpipe` it flows
// right-to-left
// rpipe is defined in the prelude as so:

(def rpipe (pipe list rev (apply pipe)))

// I will leave it as an exercise to the user to figure out how this works.
// `pipe`, `list` and `rev` are defined in the prelude, and `apply` is defined
// in core (src/Core.hs)

// ---
// Variadic Functions
// ---

// Every function is implicitly passed a list called `args`, and a list called
// `rest`. Let's make an example.

// |-----create function-----| |-call-|
  ((\ (a b) args rest (- a b)) 1 2 3 4)
//           |     |
//    (1 2 3 4)   (3 4)

// Here we see that `args` is passed a list of all the arguments applied to the
// function, and `rest` is passed all the arguments that weren't covered by the
// explicitly referenced ones (a and b).

// ---
// Recursive Lambdas
// ---

// Anonymous functions are passed themselves through the variables `rec` and
// `this`. Here's an example.

(def sum (\(lst) (if (= lst ()) 0 (+ (car lst) (rec (cdr lst))))))

// Of course, the better way to create the sum function is:
(def sum (fold 0 +))
```

For more extensive examples, see my [tests](test/eq.scm), which are written in Phage.
A lot of the functions tested are defined in the [prelude](prelude/prelude.scm).
