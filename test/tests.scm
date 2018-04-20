(import "prelude/prelude.scm")

(fun fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))

(def tlst (list 1 2 3 4))

(def tests
	(quote (

		// Core

		((= 2 2) true)
		((= 3 4) false)
		((= 2 true) false)

		((< 3 4) true)
		((< 4 3) false)
		((< 4 4) false)

		((+ 1 2) 3)

		((- 40 20) 20)
		((- 3 4) (- 1 2))

		((* 3 3) 9)
		((* 0 9) 0)

		((/ 10 4) 2)
		((/ 9 3) 3)

		((% 10 3) 1)
		((% 10 5) 0)

		((> 3 4) false)

		((<= 3 3) true)
		((>= 3 4) false)
		((list 1 2 3) (quote (1 2 3)))
		((list 1 2 3 (list 4 5) 6 7) (quote (1 2 3 (4 5) 6 7)))
		((cons 1 ()) (quote (1)))
		((cons 3 (list 4 5 6)) (quote (3 4 5 6)))
		((car (list 3 4 5)) 3)
		((cdr (list 3 4 5 6)) (quote (4 5 6)))
		((cadadr (list (list 2 (list 4 5 6)))) (quote (5 6)))
		((fact 5) 120)
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

(fun len (lst)
	(fun lrec (acc l)
		(if (= l ())
			acc
			(lrec (+ acc 1) (cdr l))))
	(lrec 0 lst))

(def cases (len tests))

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
(print (quote cases:) cases)
(print (quote passed:) passed)
(print (quote failed:) (- cases passed))
