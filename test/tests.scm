(print "importing prelude")

(import "prelude/prelude.scm")

(def tlst (list 1 2 3 4))

(print "creating test structure")

(def tests
	(quote (

		// Core

		((= 2 2) true)
		((= 3 4) false)
		((= 2 true) false)
		((= 1 1 1 1 1) true)
		((= 1 1 1 2 1) false)

		// check the core aliases work
		((eq 1 1) true)
		((eq 1 2) false)

		((& false false) false)
		((& false true) false)
		((& true false) false)
		((& true true) true)

		((| false false) false)
		((| false true) true)
		((| true false) true)
		((| true true) true)

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

		((cons 1 ()) (quote (1)))
		((cons 3 (list 4 5 6)) (quote (3 4 5 6)))

		((car (list 3 4 5)) 3)

		((cdr (list 3 4 5 6)) (quote (4 5 6)))

		((cadadr (list (list 2 (list 4 5 6)))) (quote (5 6)))

		((list 1 2 3) (quote (1 2 3)))
		((list 1 2 3 (list 4 5) 6 7) (quote (1 2 3 (4 5) 6 7)))

		((rev (list 1 2 3)) (list 3 2 1))
		((map (+ 1) tlst) (list 2 3 4 5))
		((sum tlst) 10)
		((prod tlst) 24)
		((len tlst) 4)
		((filter (< 2) tlst) (list 3 4))
		((range 1 5 1) tlst)
		((append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
		((intersperse 0 tlst) (list 1 0 2 0 3 0 4))
		((concat (list tlst tlst tlst)) (list 1 2 3 4 1 2 3 4 1 2 3 4))
		((intercalate (list 0 5) (list tlst tlst)) (list 1 2 3 4 0 5 1 2 3 4))
		((last tlst) 4)
		((init tlst) (list 1 2 3))
		((all tlst) true)
		((all (cons false tlst)) false)
		((any tlst) true)
		((any (list 0 false ())) false)
	)))

(print "getting length of cases")

(def cases (len tests))

(print "running tests")

(fun run (t)
	(fun runrec (n a t)
		(print n)
		(if (= t ())
			a
			(runrec
				(+ 1 n)
				(+ a (if (= (eval (caar t)) (eval (cadar t)))
					1
					(do
					  (print (car t))
					  (print (eval (caar t)))
					  (print (eval (cadar t)))
					  0)
				))
				(cdr t))))
	(runrec 0 0 t))

(def passed (run tests))
(print)
(print "cases:" cases)
(print "passed:" passed)
(print "failed:" (- cases passed))
