(print "importing prelude")

(import "prelude/prelude.scm")

(def tlst (list 1 2 3 4))

(print "creating test structure")

(def tests
	(quote (



		// ----
		// Core
		// ----


		// bools and sanity test

		(true true)
		(false false)


		// core aliases

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

		((| false false) false)
		((| false true) true)
		((| true false) true)
		((| true true) true)

		((! t) f)
		((! f) t)
		((! ()) t)
		((! 4) f)


		// comparisons

		((< 3 4) true)
		((< 4 3) false)
		((< 4 4) false)

		((> 3 4) false)
		((> 4 3) true)
		((> 4 4) false)

		((<= 3 4) true)
		((<= 4 3) false)
		((<= 4 4) true)

		((>= 3 4) false)
		((>= 4 3) true)
		((>= 4 4) true)


		// arithmetic

		((+ 0 0) 0)
		((+ 1 1) 2)
		((+ 100 42) 142)
		((+ 100 42 3 11) 156)

		((- 0 0) 0)
		((- 93 4) 89)
		((- 5 9) (- 0 4))
		((- 5 9 8 12) (- 0 24))

		((* 0 0) 0)
		((* 0 90) 0)
		((* 1 49) 49)
		((* 32 4) 128)
		((* 1 2 3 4 5) 120)
		((* (- 0 2) 24) (- 0 48))

		((/ 0 5) 0)
		((/ 100 10) 10)
		((/ 9 4) 2)
		((/ 64 2 2 2) 8)

		((% 0 5) 0)
		((% 8 2) 0)
		((% 9 2) 1)
		((% (- 0 5) 2) 1)
		((% 1000 410 28) 12)


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

		((& f f) f)
		((& f t) f)
		((& t f) f)
		((& t t) t)

		((& t t t t f t t) f)
		((& t t t t t t t) t)

		((-> f f) t)
		((-> f t) t)
		((-> t f) f)
		((-> t t) t)

		((^ f f) f)
		((^ f t) t)
		((^ t f) t)
		((^ t t) f)

		((~= f f) t)
		((~= f t) f)
		((~= t f) f)
		((~= t t) t)


		// lists

		((list 1 2 3) (quote (1 2 3)))
		((list 1 2 3 (list 4 5) 6 7) (quote (1 2 3 (4 5) 6 7)))
		((rev (list 1 2 3)) (list 3 2 1))
		((map (+ 1) tlst) (list 2 3 4 5))
		((sum tlst) 10)
		((prod tlst) 24)
		((len tlst) 4)
		((filter (< 2) tlst) (list 3 4))
		((range 1 5 1) tlst)
		((range 1 10 3) (list 1 4 7))
		((range 10 0 (- 0 3)) (list 10 7 4 1))
		((range 10 1 (- 0 3)) (list 10 7 4))
		((append () ()) ())
		((append tlst ()) tlst)
		((append () tlst) tlst)
		((append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
		((concat ()) ())
		((concat (list tlst tlst)) (append tlst tlst))
		((intersperse 0 tlst) (list 1 0 2 0 3 0 4))
		((concat (list tlst tlst tlst)) (list 1 2 3 4 1 2 3 4 1 2 3 4))
		((intercalate (list 0 5) (list tlst tlst)) (list 1 2 3 4 0 5 1 2 3 4))
		((last tlst) 4)
		((init tlst) (list 1 2 3))
		((all tlst) true)
		((all (cons false tlst)) false)
		((any tlst) true)
		((any (list 0 false ())) false)


		// pipe

		(((pipe - (* 3) (+ 1)) 9 5 1) 10)
		(((rpipe (+ 1) (* 3) -) 9 5 1) 10)
		((min 5 6 7 78 4 10) 4)
		((min 1 6 7 78 4 10) 1)
		((min 1 6 7 78 4 10 0) 0)
		((minl (list 5 6 7 78 4 10)) 4)
		((minl (list 1 6 7 78 4 10)) 1)
		((minl (list 1 6 7 78 4 10 0)) 0)
		((max 5 6 7 18 4 10) 18)
		((max 12 6 7 8 4 10) 12)
		((max 1 6 7 7 4 10 13) 13)
		((maxl (list 5 6 7 18 4 10)) 18)
		((maxl (list 12 6 7 8 4 10)) 12)
		((maxl (list 1 6 7 7 4 10 13)) 13)


		// misc

		((do 1 2 3) 3)
	)))

(print "getting length of cases")

(def cases (len tests))

(print "running tests")

(fun run (tests)
	(fun runrec (n a test)
		(print n)
		(if (= test ())
			a
			(runrec
				(+ 1 n)
				(+ a (if (= (eval (caar test)) (eval (cadar test)))
					1
					(do
					  (print (car test))
					  (print (eval (caar test)))
					  (print (eval (cadar test)))
					  0)
				))
				(cdr test))))
	(runrec 0 0 tests))

(def passed (run tests))
(print)
(print "cases:" cases)
(print "passed:" passed)
(print "failed:" (- cases passed))
