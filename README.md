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

* define
* better prelude
* datatypes
* add some more data types
* lists and list functions
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
```

...and the following special forms:

```
// named function (supports recursion)
(fun alt (a b) (print a) (alt b a))

// anonymous function
(\ (a b c) (+ a (+ b c)))

// create a variable binding
(def a 3)

// create many variable bindings
(let (a 3) (b 100))
```

## Example

```
// factorial
(fun fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))
```
