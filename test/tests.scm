(import "prelude/prelude.scm")

(fun fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))

(def tlst (list 1 2 3 4))

(def tests
	(quote (
// 0
		((+ 1 2) 3)
		((* 3 3) 9)
		((/ 10 4) 2)
		((% 10 3) 1)
		((min 3 4) 3)
// 5
		((max 3 4) 4)
		((= 2 2) true)
		((= 2 4) false)
		((< 3 4) true)
		((> 3 4) false)
// 10
		((<= 3 3) true)
		((>= 3 4) false)
		((list 1 2 3) (quote (1 2 3)))
		((list 1 2 3 (list 4 5) 6 7) (quote (1 2 3 (4 5) 6 7)))
		((cons 1 ()) (quote (1)))
// 15
		((cons 3 (list 4 5 6)) (quote (3 4 5 6)))
		((car (list 3 4 5)) 3)
		((cdr (list 3 4 5 6)) (quote (4 5 6)))
		((cadadr (list (list 2 (list 4 5 6)))) (quote (5 6)))
		((fact 5) 120)
// 20
		((rev (list 1 2 3)) (list 3 2 1))
		((map (+ 1) tlst) (list 2 3 4 5))
		((sum tlst) 10)
		((prod tlst) 24)
		((len tlst) 4)
// 25
		((filter (< 2) tlst) (list 3 4))
		((range 1 5 1) tlst)
		((append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
		((intersperse 0 tlst) (list 1 0 2 0 3 0 4))
		((concat (list tlst tlst tlst)) (list 1 2 3 4 1 2 3 4 1 2 3 4))
// 30
		((intercalate (list 0 5) (list tlst tlst)) (list 1 2 3 4 0 5 1 2 3 4))
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
