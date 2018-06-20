<div align="center">
	<p>
	   <img src="http://owenowen.netsoc.ie/res/phage.svg" height="150">
	</p>
	<h1>The Phage Programming Language</h1>
	<a href="https://travis-ci.org/414owen/Phage">
		<img
			src="https://img.shields.io/travis/414owen/Phage.svg"
			alt="Build Status Badge"
		></img>
	</a>
	<img
		src="https://img.shields.io/github/license/414owen/Phage.svg"
		alt="Licence Badge"
	></img>
</div>


Phage is a functional, syntactically pure, interpreted programming language.
It aims to provide maximum expressivity, excellent support for metaprogramming,
and be 'hackable'.

## Features

### Redefine Everything

Phage has no keywords. If you want, you can redefine `if`, `true`, and `def`.
This is because `if`, `true`, and `def` are normal bindings.

```
(def a 1)    // a becomes 1
(def d def)  // d becomes def
(d b 4)      // b becomes 4
(d def 4)    // def becomes 4
(def c 5)    // broken! you can't call 4!
```

Because of this property, it's very easy to project your personal taste onto
Phage. There are other ways to define bindings, which we'll cover later on.

### Immutability

Data is immutable. When we redefine variables, everything up to that point will
maintain the old reference. Here is an example (notice that when we redefine
`a`, `pr` maintains the old reference):

```
(def a 1)              // a becomes 1
(print a)              // prints 1
(fn pr () (print a))   // defines a function that prints a
(def a 2)              // a becomes 2
(print a)              // prints 2
(pr)                   // prints 1
```

This is different from variable shadowing, which only occurs on nested scopes.
In Phage, functions are bound to their contexts (everything defined above
them), and ignore bindings defined below, and in scopes more nested than them.

### Syntactic Purity

* A program consists of many lists
* Lists are evaluated by taking the first element (the `car`), which should
  be a function / special form, and applying the tail (the `cdr`) to it.

### Homoiconicity

As is common with languages that have lots of brackets, Phage is homoiconic.
This means that Phage's Abstract Syntax Tree is the same as the textual layout
of the language. The form `quote` allows you to convert program segments to
data, and `eval` does the converse.

```
(def a (quote (+ 1 2 3)))  // (+ 1 2 3)
a                          // (+ 1 2 3)
(eval a)                   // 6
(a)                        // error: (+ 1 2 3) is not a function
(car a)                    // +
(cdr a)                    // (1 2 3)
```

### Many types of evaluatables

| Named   | Lambda   | Evaluates Args   | Scoping   | Creates Scope |
| ------- | -------- | ---------------- | --------- | ------------- |
| `fn`    | `\`      | yes              | lexical   | yes           |
| `sfn`   | `s\`     | yes              | lexical   | no            |
| `dfn`   | `d\`     | yes              | dynamic   | yes           |
| `dsfn`  | `ds\`    | yes              | dynamic   | no            |
| `fm`    | `\\`     | no               | lexical   | yes           |
| `sfm`   | `s\\`    | no               | lexical   | no            |
| `dfm`   | `d\\`    | no               | dynamic   | yes           |
| `dsfm`  | `ds\\`   | no               | dynamic   | no            |

#### Argument Evaluation

Functions evaluate their arguments, forms don't. Forms take in the AST
representation of their arguments. The simplest example of this is the `quote`
function we saw above. It is defined as follows:

```
(def quote (\\(a) a))

// or

(fm quote (a) a)
```

#### Scoping

Lexically scoped evaluatables use the context present when they were defined.
Dynamically scoped evaluatables use the context of the caller.

```
(fn lex () a)
(dfn dyn () a)
(def a 3)
(lex)          // error: variable 'a' not in scope
(dyn)          // 3
```

#### Scope Creation

Finally, some evaluatables create their own scope, whereas some use the
surrounding scope. For the former, variable declarations are local to the body
of the evaluatable, which is 'normal'.

```
((\ () (def a 3)))    // extra brackets used to perform call immediately
((s\ () (def b 4)))   // this one doesn't create its own scope

b     // 4
a     // error: variable 'a' not in scope
```

#### Why is this Useful?

Various combinations of the above evaluatables are necessary to abstract some
special forms. For example, there is one inbuilt form, `def`, which adds an
entry to the symbol table. In the prelude (written in Phage), there is a
function `defs`, which adds multiple entries to the symbol table. It is defined
and used as follows:

```
(dsfm defs () (map (apply def) args))

(defs
	(a 1)
	(b (+ 1 1))
	(c 3))

a     // 1
b     // 2
c     // 3
```

The named evaluatable definers, `fn`, `fm`, `dsfm`, etc. are also defined in
the prelude, using the above features. Only the lambda forms of each are written
in Haskell.

## Usage

```bash
$ stack build                    # to build
$ stack exec phage               # run the repl
$ stack exec phage -- <program>  # run a Phage program
```

## Examples

```
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

(+ 1 2)       // 3
(+ 1 2 3 4 5) // 15

// ---
// Function Definition
// ---

// `hello` takes two parameters, prints them, then returns their sum
(fn hello (a b) (print a b) (+ a b))

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
(fn fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))

// product of range
(fn fact (a) (prod (upto a)))

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
// reading their implementation in `prelude/prelude.scm`.
// They're all very simple

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

// ---
// List Comprehensions
// ---

// List comprehensions are defined in Phage, in `stdlib/comp.scm`.
// There is no 'glue' syntax, like `[_ for _ in _]` or `[_ | _ <- |]`.
// They also read left-to-right. Here are some examples:

(comp (list 1 2 3) x x)                       // (1 2 3)
(comp (list 1 2 3) x (list 4 5 6) y (* x y))  // (4 5 6 8 10 12 12 15 18)
```

For more extensive examples, see my [tests](test/eq.scm), which are written in Phage.
A lot of the functions tested are defined in the [prelude](prelude/prelude.scm).
