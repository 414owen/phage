# The Phage Programming Language

## Usage

```bash
$ stack build        # to build
$ stack exec phage   # run the repl
```

## Syntax

* A program consists of many calls
* Calls consist of a function / special form and its arguments, surrounded by
	brackets: `(<func> <param1> <param2> .. <paramN>)`

## Prelude

The prelude includes the following constants:

```
λ: ()
>> ()           // the empty list
λ: true
>> true
λ: false
>> false
λ: zero
>> 0
```

The following functions:

```
λ: (+ 1 2)
>> 3
λ: (- 1 2)
>> -1
λ: (* 3 3)
>> 9
λ: (/ 10 4)
>> 2
λ: (% 10 3)
>> 1

λ: (min 3 4)
>> 3
λ: (max 3 4)
>> 4

λ: (= 2 2)
>> true
λ: (= 2 4)
>> false
λ: (< 3 4)
>> true
λ: (> 3 4)
>> false
λ: (<= 3 3)
>> true
λ: (>= 3 4)
>> false

// prints (and returns) 5
λ: (print 5)
>> 5

// `list` creates a list from its parameters
λ: (list 1 2 3)
>> (1 2 3)
λ: (list (1 2 3 (list 4 5) 6 7)
>> (1 2 3 (4 5) 6 7)

// `cons` adds an item to the start of a list
λ: (cons 1 ())
>> (1)
λ: (cons 3 (list 4 5 6))
>> (3 4 5 6)

// `car` returns the first item in a list
λ: (car (list 3 4 5))
>> 3

// `cdr` returns every element except the first
λ: (cdr (list 3 4 5 6))
>> (4 5 6)

// there are other, similar functions, that
// combine `car` and `cdr`. You can figure
// out what they do by following the 'a's
// and 'd's from left to right. For example:
λ: (cadadr (list (list 2 (list 4 5 6))))
>> (5 6)

// These functions are defined up to eight-character
// functions such as `caaaddar` and `cdadaadr`.
// If you are dealing with lists of higher dimensionality
// than six, you're on your own.
```

The following special forms:

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
