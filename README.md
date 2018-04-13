# The Phage Programming Language

## Usage

```bash
$ stack build        # to build
$ stack exec phage   # run the repl
```

## Implemented

* Arithmetic
* Function Application
* REPL

## TODO

* better prelude
* datatypes
* lists
* everything else

## Syntax

* A program consists of many calls
* Calls consist of a function / special form and its arguments, surrounded by
	brackets: `(<func> <param1> <param2> .. <paramN>)`

## Prelude

The prelude includes the following functions:

```
(+ 1 2)      //  3
(- 1 2)      // -1
(* 3 3)      //  9
(/ 10 4)     //  2
(% 10 3)     //  1

(min 3 4)    //  3
(max 3 4)    //  4

(= 2 2)      // true
(= 2 4)      // false
(< 3 4)      // true
(> 3 4)      // false
(<= 3 3)     // true
(>= 3 4)     // false

// prints (and returns) 5
(print 5)
```

...and the following special forms:

```
// define a function
// also returns the function literal
(fun alt (a b) (print a) (alt b a))

// anonymous function
(\ (a b c) (+ a (+ b c)))

// create a variable binding
// returns the variable
(def a 3)

// create many variable bindings
// returns nil
(let (a 3)
	 (b 100))

// (if <cond> <then> <else>)
// returns the result of the taken branch
(if (< a b)
	(* b b)
	(* a b))

// cond takes the first element of each of its parameters
// as a condition, returning the second if it's true
(cond
	(false 1)
	((> 3 4) 2)
	((= 3 3) 3)
	(true 4))
```

## Examples

```
// factorial
(fun fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))

// currying
(def add3 (+ 3))
(add3 5)            // 8
```
