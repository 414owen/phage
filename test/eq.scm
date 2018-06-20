(import "prelude/wordy.scm")

(def tests
	(quote (



		// ----
		// Core
		// ----


		// bools and sanity test

		(true true)
		(false false)


		// redefinitions

		(false f)
		(t true)


		// equality

		((= 2 2) true)
		((= 3 4) false)
		((= 2 true) false)
		((= 1 1 1 1 1) true)
		((= 1 1 1 2 1) false)


		// expression nesting

		((eq 5 (+ 2 3)) true)
		((eq 5 (+ 3 3)) false)


		// boolean logic

		// or
		((| false false) false)
		((| false true) true)
		((| true false) true)
		((| true true) true)
		((| 1 2 3) 1)
		((| () () 0 false "hi" 6 7 true 0) "hi")

		// not
		((! t) f)
		((! f) t)
		((! ()) t)
		((! 4) f)


		// comparisons

		// less than
		((< 3 4) true)
		((< 4 3) false)
		((< 4 4) false)

		// greater than
		((> 3 4) false)
		((> 4 3) true)
		((> 4 4) false)

		// less than or equal to
		((<= 3 4) true)
		((<= 4 3) false)
		((<= 4 4) true)

		// greater than or equal to
		((>= 3 4) false)
		((>= 4 3) true)
		((>= 4 4) true)


		// arithmetic

		// addition
		((+ 0 0) 0)
		((+ 1 1) 2)
		((+ 100 42) 142)
		((+ 100 42 3 11) 156)

		// negation
		((- 0 0) 0)
		((- 93 4) 89)
		((- 5 9) (- 0 4))
		((- 5 9 8 12) (- 0 24))

		// multiplication
		((* 0 0) 0)
		((* 0 90) 0)
		((* 1 49) 49)
		((* 32 4) 128)
		((* 1 2 3 4 5) 120)
		((* (- 0 2) 24) (- 0 48))

		// division
		((/ 0 5) 0)
		((/ 100 10) 10)
		((/ 9 4) 2)
		((/ 64 2 2 2) 8)

		// modulo
		((% 0 5) 0)
		((% 8 2) 0)
		((% 9 2) 1)
		((% (- 0 5) 2) 1)
		((% 1000 410 28) 12)


		// if

		// returns the correct result
		((if true 1 2) 1)
		((if false 1 2) 2)

		// evaluates the condition
		((if (+ 1 2) 1 2) 1)
		((if (- 1 1) 1 2) 2)


		// abstraction

		// functions return results
		(((\ () 3)) 3)

		// functions evaluate their arguments
		(((\ (a) a) ((\ () 3))) 3)

		// unscoped abstractions allow definitions to carry
		(((s\ () (def hello "hi"))) "hi")
		(((s\ () (def hello "hey"))) hello)
		(((s\\ () (def hello "hi"))) hello)

		// forms return values
		(((\\ () 3)) 3)

		// forms don't evaluate their argumants
		(((\\ (a) ((eval a))) (\ () 3)) 3)
		(((\\ (a) (cdar a)) (\ () 3)) ())
		(((\\ (a) (cddar a)) (\ () 3)) 3)

		// lambda recursion
		(((\ (a) (if (= a 0) () (cons a (rec (- a 1))))) 4) (list 4 3 2 1))
		(((\ (a) (if (= a 0) () (cons a (this (- a 1))))) 4) (list 4 3 2 1))


		// meta

		// eval on simple values
		((eval 4) 4)
		((eval "hi") "hi")

		// eval on expressions
		((eval (quote (+ 1 2))) 3)
		((do (def a (quote (+ 1 2))) (eval a)) 3)

		// arity
		((arity (\ () ())) 0)
		((arity (\ (a b c) ())) 3)

		// apply orders parameters correctly
		((apply (\ (a b c) c) (quote (1 2 3))) 3)

		// apply allows scope changes
		((apply (s\ () (def applyscope 4)) ()) applyscope)

		// call orders parameters correctly
		((call (\ (a b c) c) 1 2 3) 3)

		// call allows scope changes
		((call (s\ () (def callscope 4)) ()) callscope)

		// define returns a value
		((def a 100) 100)

		// define edits the scope
		((def defscope 1000) defscope)


		// lists

		((cons 1 ()) (quote (1)))
		((cons 3 (list 4 5 6)) (quote (3 4 5 6)))
		((car (list 3 4 5)) 3)
		((cdr (list 3 4 5 6)) (quote (4 5 6)))
		((cadadr (list (list 2 (list 4 5 6)))) (quote (5 6)))



		// -------
		// prelude
		// -------


		// boolean logic

		// and
		((& f f) f)
		((& f t) f)
		((& t f) f)
		((& t t) t)

		((& t t t t f t t) f)
		((& t t t t t t t) t)

		// implication
		((-> f f) t)
		((-> f t) t)
		((-> t f) f)
		((-> t t) t)

		// xor
		((^ f f) f)
		((^ f t) t)
		((^ t f) t)
		((^ t t) f)

		// equivalence
		((~= f f) t)
		((~= f t) f)
		((~= t f) f)
		((~= t t) t)


		// quoting
		((quotes 3 4 (1 2)) (quote (3 4 (1 2))))
		((quote (1 2 3)) (list 1 2 3))


		// evaluatable shorthands

		// are evaluatable
		(((fn hello () "hi")) "hi")

		// add to the symbol table
		((do (fn a () "hi") (a)) "hi")


		// lists

		// creation
		((list 1 2 3) (quote (1 2 3)))
		((list 1 2 3 (list 4 5) 6 7) (quote (1 2 3 (4 5) 6 7)))
		((range 1 5 1) tlst)
		((range 1 10 3) (list 1 4 7))
		((range 10 0 (- 0 3)) (list 10 7 4 1))
		((range 10 1 (- 0 3)) (list 10 7 4))
		((upto 3) (list 1 2 3))

		// reversal
		((rev (list 1 2 3)) (list 3 2 1))

		// mapping
		((map (+ 1) tlst) (list 2 3 4 5))

		// summation
		((sum tlst) 10)

		// product
		((prod tlst) 24)

		// length
		((len tlst) 4)

		// filtration
		((filter (< 2) tlst) (list 3 4))

		// joining
		((append () ()) ())
		((append tlst ()) tlst)
		((append () tlst) tlst)
		((append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
		((concat ()) ())
		((concat (list tlst tlst)) (append tlst tlst))
		((intersperse 0 tlst) (list 1 0 2 0 3 0 4))
		((concat (list tlst tlst tlst)) (list 1 2 3 4 1 2 3 4 1 2 3 4))
		((intercalate (list 0 5) (list tlst tlst)) (list 1 2 3 4 0 5 1 2 3 4))
		((push (list 1 2 3) 4) (list 1 2 3 4))
		((push (list 1 2 3) 4 5 6) (list 1 2 3 4 5 6))

		// drop last
		((init tlst) (list 1 2 3))

		// access
		((last tlst) 4)

		// element-wise tests
		((all tlst) 4)
		((all (cons false tlst)) false)
		((any tlst) 1)
		((any (list 0 false ())) ())

		// nesting
		((nest 0 ()) ())
		((nest 3 4) (list (list (list 4))))

		// zipping
		((zip (list 1 2 3) (list 4 5 6)) (quote ((1 4) (2 5) (3 6))))
		((zip (list 1 2 3) (list 4 5 6) (list 7 8 9))
			(quote ((1 4 7) (2 5 8) (3 6 9))))

		// transpose
		((transpose (quote ((1 2) (3 4) (5 6))))
			(quote ((1 3 5) (2 4 6))))

		// rotate
		((rotate (quote ((1 2) (3 4) (5 6))))
			(quote ((2 4 6) (1 3 5))))

		// function wrappers
		(((const 4) 3 5 6) 4)
		(((flip (\ (a b) a)) 1 2) 2)

		// pipe
		(((pipe - (* 3) (+ 1)) 9 5 1) 10)
		(((rpipe (+ 1) (* 3) -) 9 5 1) 10)

		// min/max
		((minl (list 5 6 7 78 4 10)) 4)
		((minl (list 1 6 7 78 4 10)) 1)
		((minl (list 1 6 7 78 4 10 0)) 0)
		((maxl (list 5 6 7 18 4 10)) 18)
		((maxl (list 12 6 7 8 4 10)) 12)
		((maxl (list 1 6 7 7 4 10 13)) 13)


		// homogeneous binary functions

		((min 5 6 7 78 4 10) 4)
		((min 1 6 7 78 4 10) 1)
		((min 1 6 7 78 4 10 0) 0)
		((max 5 6 7 18 4 10) 18)
		((max 12 6 7 8 4 10) 12)
		((max 1 6 7 7 4 10 13) 13)


		// forms

		// work
		(((\ () (defs (a 3) (b 4) (c 1)) (+ a b c))) 8)

		// can evaluate parameters
		(((\ () (defs (a 3) (b (* 2 2)) (c 1)) (+ a b c))) 8)

		// scope carries with custom forms
		(((s\ () (defs (a 3) (b 4) (c 1)) 8)) (+ a b c))
		((let (a 10) a) 10)
		((lets ((a 11) (b 12)) 3 (+ a b)) 23)

		// misc

		((do 1 2 3) 3)



		// ------
		// stdlib
		// ------

		((comp (list 1 2 3 4 5) x x) (list 1 2 3 4 5))
		((comp (list 1 2 3) x (list 4 5 6) y (list x y))
			(quote ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))))
		((comp (list 1 2 3) x (list 4 5 6) y (* x y))
			(list 4 5 6 8 10 12 12 15 18))
	)))

